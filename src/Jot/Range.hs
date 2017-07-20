module Jot.Range
  ( Range(..)
  , size
  , ofPoint
  ) where

import Data.Semigroup ((<>))

-- |
data Range = Range Int Int
  deriving Eq

-- |
size :: Range -> Int
size (Range i j) = j - i

-- |
ofPoint :: Int -> Range
ofPoint x = Range x x

instance Show Range where
  show (Range i j) = "[" <> show i <> "," <> show j <> ")"
