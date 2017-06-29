----------------------------------------------------------------------------
-- |
-- Module      :  Application.Git.Errors
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  27 January 2017
-- Stability   :
-- Portability :
--
-- Most of the errors thrown by git are collected in this mode. The approach
-- to error handling is to use mtl-style "MonadError e m" monad and add
-- constraints on "e" parameter in a finally-tagless style. This allows
-- different error types to combine nicely.
----------------------------------------------------------------------------

module Application.Git.Errors
  ( FileNotFoundError(..)
  , InvalidHEADError(..)
  , InvalidObjectType(..)
  ) where

import qualified Data.ByteString.Char8 as C8

import Data.Git.BlobType
import Data.Git.SHAHash

-- | Generic "file not found" error.
class FileNotFoundError a where
  mkFileNotFoundError
    :: FilePath -- ^ File that is missing.
    -> a

-- | The "HEAD" file has invalid format.
class InvalidHEADError a where
  mkInvalidHEADError
    :: C8.ByteString -- ^ Contents of ".git/HEAD" file.
    -> a

class InvalidObjectType a where
  mkInvalidObjectTypeError
    :: BlobType -- ^ Expected object type
    -> BlobType -- ^ Real object type
    -> SHAHash' -- ^ SHA of offending object
    -> a
