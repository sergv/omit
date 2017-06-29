----------------------------------------------------------------------------
-- |
-- Module      :  Data.Git.SHAHash
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  26 January 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Data.Git.SHAHash
  ( SHAHash
  , SHAHash'
  , gitObjectPath
  , unSHAHash'
  , showSHA
  , readSHA
  , showSHA'
  , readSHA'
  , hashobj
  , hashobj'
  , InvalidSHAError(..)
  , shaFromByteString
  , pSHAHash
  ) where

import Control.Monad.Except
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Digest.Pure.SHA as SHA
import Data.Word
import Text.Printf

import Data.Char.Ext

type SHAHash = B.ByteString

showSHA :: SHAHash -> String
showSHA = concatMap (printf "%02x") . B.unpack

{-# WARNING readSHA "readSHA function is not total" #-}
readSHA :: String -> SHAHash
readSHA str =
  B.pack $
  splitBy2
    (fromRight . readHexSingleton str)
    (\c1 c2 -> fromRight $ readHexPair str c1 c2)
    str
  where
    fromRight :: Either String a -> a
    fromRight (Right x)  = x
    fromRight (Left msg) = error $ "SHAHash.readSHA.fromRight: failed: " ++ msg


data SHAHash' = SHAHash' !Word8 !B.ByteString
  deriving (Eq, Ord, Show)

gitObjectPath :: SHAHash' -> FilePath
gitObjectPath (SHAHash' w ws) =
  concat ["objects/", showHexByte w, "/", C8.unpack ws]

unSHAHash' :: SHAHash' -> B.ByteString
unSHAHash' (SHAHash' w ws) = B.cons w ws

showSHA' :: SHAHash' -> String
showSHA' = showSHA . unSHAHash'

readSHA' :: String -> Either String SHAHash'
readSHA' = \case
  str@(c1:c2:cs) ->
    SHAHash'
      <$> readHexPair str c1 c2
      <*> (B.pack <$> sequence (splitBy2 (readHexSingleton str) (readHexPair str) cs))
  str         ->
    Left $ "SHA hash is too short, at least two characters expected but found '" ++ str ++ "'"

hashobj :: BL.ByteString -> SHAHash
hashobj = BL.toStrict . SHA.bytestringDigest . SHA.sha1

hashobj' :: BL.ByteString -> SHAHash'
hashobj' = fromBS . BL.toStrict . SHA.bytestringDigest . SHA.sha1

-- Internal helper.
fromBS :: B.ByteString -> SHAHash'
fromBS str = SHAHash' (BU.unsafeHead str) (BU.unsafeTail str)

class InvalidSHAError a where
  mkInvalidSHAError :: B.ByteString -> a

-- | Read bytestring as a hash.
shaFromByteString :: (InvalidSHAError e, MonadError e m) => B.ByteString -> m SHAHash'
shaFromByteString str
  | B.length str >= 2 && C8.all isHexChar str
  = pure $ fromBS str
  | otherwise
  = throwError $ mkInvalidSHAError str

pSHAHash :: Atto.Parser SHAHash'
pSHAHash = SHAHash' <$> (fromIntegral . ord <$> Atto.satisfy isHexChar) <*> Atto.takeWhile isHexChar

-- Utils

splitBy2 :: (a -> b) -> (a -> a -> b) -> [a] -> [b]
splitBy2 onLast onPair = go
  where
    go []         = []
    go [x]        = [onLast x]
    go (x1:x2:xs) = onPair x1 x2 : go xs

readHexSingleton :: String -> Char -> Either String Word8
readHexSingleton str c =
  case readHexChar c of
    Just x -> Right x
    Nothing ->
      Left $ "Invalid hex numeral '" ++ [c] ++ "' in sha hash: '" ++ str ++ "'"

readHexPair :: String -> Char -> Char -> Either String Word8
readHexPair str c1 c2 =
  case (\x1 x2 -> x1 * 16 + x2) <$> readHexChar c1 <*> readHexChar c2 of
    Just x  -> Right $! x
    Nothing ->
      Left $ "Invalid hex numeral '" ++ [c1, c2] ++ "' in sha hash: '" ++ str ++ "'"

showHexByte :: Word8 -> String
showHexByte w = [showHexChar w1, showHexChar w2]
  where
    (w1, w2) = quotRem w 16
