module Main (main) where

import System.Environment (getArgs)

import Application.Git

main :: IO ()
main = getArgs >>= runGit
