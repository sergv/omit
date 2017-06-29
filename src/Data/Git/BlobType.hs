----------------------------------------------------------------------------
-- |
-- Module      :  Data.Git.BlobType
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  31 January 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Data.Git.BlobType
  ( BlobType(..)
  , blobTypeArgumentParser
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Options.Applicative

data BlobType = BlobCommit | BlobTree | BlobObject | BlobTag
  deriving (Bounded, Enum, Eq, Ord, Show)

blobTypes :: Map String BlobType
blobTypes = M.fromList
  [ ("commit", BlobCommit)
  , ("tree",   BlobTree)
  , ("object", BlobObject)
  , ("tag",    BlobTag)
  ]

readBlobType :: String -> Either String BlobType
readBlobType x = case M.lookup x blobTypes of
  Just x' -> Right x'
  Nothing -> Left $ "Invalid blob type: '" ++ x ++ "'"

blobTypeArgumentParser :: Parser BlobType
blobTypeArgumentParser =
  argument
    (eitherReader readBlobType)
    (metavar "<type>" <>
     completeWith (M.keys blobTypes))

