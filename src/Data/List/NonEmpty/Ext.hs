----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.NonEmpty.Ext
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  29 January 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Data.List.NonEmpty.Ext
  ( someNE
  , module Data.List.NonEmpty
  ) where

import Control.Applicative
import Data.List.NonEmpty

someNE :: Alternative f => f a -> f (NonEmpty a)
someNE f = (:|) <$> f <*> many f
