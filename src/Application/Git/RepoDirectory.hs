----------------------------------------------------------------------------
-- |
-- Module      :  Application.Git.RepoDirectory
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  27 January 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Application.Git.RepoDirectory
  ( RepoDirectory
  , unRepoDirectory
  , NoRepoDirectoryError(..)
  , findRepoDirectory
  ) where

import Control.Monad.Except
import Control.Monad.Extra (firstJustM)
import qualified Data.List as L
import System.Directory
import System.FilePath

-- | No ".git" directory was found
class NoRepoDirectoryError a where
  mkNoRepoDirectoryError
    :: FilePath -- ^ Directory where search started from.
    -> a

-- | Existing ".git" directory.
newtype RepoDirectory = RepoDirectory { unRepoDirectory :: FilePath }
  deriving (Eq, Ord, Show)

-- | Search for ".git" directory the current directory belongs to.
findRepoDirectory
  :: (NoRepoDirectoryError e, MonadError e m, MonadIO m)
  => FilePath -- ^ Current directory
  -> m RepoDirectory
findRepoDirectory currentDir = do
  res <- flip firstJustM parents $ \path -> do
    exists <- liftIO $ doesDirectoryExist path
    pure $
      if exists
      then Just $ RepoDirectory path
      else Nothing
  case res of
    Nothing  -> throwError $ mkNoRepoDirectoryError currentDir
    Just dir -> pure dir
  where
    parents :: [FilePath]
    parents = map ((</> ".git") . joinPath)
            $ filter (not . null)
            $ L.tails
            $ splitPath currentDir
