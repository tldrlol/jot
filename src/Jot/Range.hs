module Jot.Range
  ( Range(..)
  , range
  ) where

import Data.Semigroup ((<>))

-- |
data Range = Range
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int

-- |
range :: Int -> Int -> Range
range a b | a <= b    = Range a b
          | otherwise = Range b a

instance Show Range where
  show (Range i j) = "[" <> show i <> ", " <> show j <> ")"
