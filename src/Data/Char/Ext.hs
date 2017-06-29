----------------------------------------------------------------------------
-- |
-- Module      :  Data.Char.Ext
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

{-# LANGUAGE LambdaCase #-}

module Data.Char.Ext
  ( isNewline
  , isHexChar
  , readHexChar
  , showHexChar
  , module Data.Char
  ) where

import Data.Char
import Data.Maybe
import Data.Word

{-# INLINABLE isNewline #-}
isNewline :: Char -> Bool
isNewline = \case
  '\n' -> True
  '\r' -> True
  _    -> False

isHexChar :: Char -> Bool
isHexChar = isJust . readHexChar

readHexChar :: Char -> Maybe Word8
readHexChar = \case
  '0' -> Just 0x0
  '1' -> Just 0x1
  '2' -> Just 0x2
  '3' -> Just 0x3
  '4' -> Just 0x4
  '5' -> Just 0x5
  '6' -> Just 0x6
  '7' -> Just 0x7
  '8' -> Just 0x8
  '9' -> Just 0x9
  'a' -> Just 0xa
  'b' -> Just 0xb
  'c' -> Just 0xc
  'd' -> Just 0xd
  'e' -> Just 0xe
  'f' -> Just 0xf
  'A' -> Just 0xa
  'B' -> Just 0xb
  'C' -> Just 0xc
  'D' -> Just 0xd
  'E' -> Just 0xe
  'F' -> Just 0xf
  _   -> Nothing

showHexChar :: Word8 -> Char
showHexChar = \case
  0x0 -> '0'
  0x1 -> '1'
  0x2 -> '2'
  0x3 -> '3'
  0x4 -> '4'
  0x5 -> '5'
  0x6 -> '6'
  0x7 -> '7'
  0x8 -> '8'
  0x9 -> '9'
  0xa -> 'a'
  0xb -> 'b'
  0xc -> 'c'
  0xd -> 'd'
  0xe -> 'e'
  0xf -> 'f'
  c   -> error $ "invalid hex char: '" ++ [chr $ fromIntegral c] ++ "'"

