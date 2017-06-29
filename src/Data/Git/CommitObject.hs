----------------------------------------------------------------------------
-- |
-- Module      :  Data.Git.CommitObject
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  28 January 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.Git.CommitObject
  ( CommitObject(..)
  , MalformedCommitObjectError(..)
  , parseCommitObject
  , parseCommitObjectFromLazy
  ) where

import Control.Applicative
import Control.Monad.Except
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AttoLazy
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty(..))

import Data.Char.Ext (isNewline)
import Data.Git.SHAHash
import Data.List.NonEmpty.Ext

data CommitObject = CommitObject
  { commitObjectTree         :: SHAHash'
  , commitObjectParent       :: NonEmpty SHAHash'
  , commitObjectAuthor       :: B.ByteString
  , commitObjectCommitter    :: B.ByteString
  , commitObjectGPGSignature :: Maybe B.ByteString
  , commitObjectMessage      :: B.ByteString
    -- [("tree", showSHA treesha),("parent", prevcommit),("author", cmAuthor),("committer", cmAuthor)]
  }

class MalformedCommitObjectError a where
  mkMalformedCommitObjectError
    :: String -- ^ Error message
    -> B.ByteString -- ^ Malformed contents.
    -> a

parseCommitObject
  :: (MalformedCommitObjectError e, MonadError e m)
  => B.ByteString
  -> m CommitObject
parseCommitObject blob =
  case parseOnly (pCommitObject <* endOfInput) blob of
    Left msg -> throwError $ mkMalformedCommitObjectError msg blob
    Right x  -> pure x

parseCommitObjectFromLazy
  :: (MalformedCommitObjectError e, MonadError e m)
  => BL.ByteString
  -> m CommitObject
parseCommitObjectFromLazy blob =
  case AttoLazy.eitherResult $ AttoLazy.parse (pCommitObject <* endOfInput) blob of
    Left msg -> throwError $ mkMalformedCommitObjectError msg $ BL.toStrict blob
    Right x  -> pure x


pCommitObject :: Parser CommitObject
pCommitObject = CommitObject
  <$> block "tree" pSHAHash
  <*> someNE (block "parent" pSHAHash)
  <*> block "author" (takeTill isNewline)
  <*> block "committer" (takeTill isNewline)
  <*> optional (mconcat <$> multilineBlock "gpgsig" (takeTill isNewline))
  <*> (endOfLine *> takeByteString)
  where
    block :: B.ByteString -> Parser a -> Parser a
    block name bodyParser = string name *> void space *> bodyParser <* endOfLine
    multilineBlock :: B.ByteString -> Parser a -> Parser [a]
    multilineBlock name bodyParser =
      string name *>
      (void space *> sepBy1 bodyParser (endOfLine *> void space)) <*
      endOfLine

