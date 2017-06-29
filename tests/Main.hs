----------------------------------------------------------------------------
-- |
-- Module      :  Main
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

module Main (main) where

import Test.Tasty

import qualified Data.Git.CommitObject.Tests as CommitObjectTests

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ CommitObjectTests.tests
  ]
