{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Jot.History
  ( History
  , singleton
  , view
  , record
  , up
  , down
  , right
  , left
  ) where

import           Data.List (uncons)
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE

-- |
data Tree a = Tree
  { content  :: a
  , leading  :: {-# UNPACK #-} ![Tree a]
  , trailing :: {-# UNPACK #-} ![Tree a]
  } deriving (Eq, Foldable, Functor, Show)

-- |
singletonTree :: a -> Tree a
singletonTree a = Tree a [] []

-- |
push :: Tree a -> Tree a -> Tree a
push a t = t { trailing = a : trailing t }

-- |
pop :: Tree a -> Maybe (Tree a, Tree a)
pop (Tree a l ts) = uncons ts >>= \(t',ts') -> pure (t', Tree a l ts')

-- |
focusRight :: Tree a -> Maybe (Tree a)
focusRight x = do
  (t, ts) <-  uncons (trailing x)
  pure $ x { leading = t : leading x, trailing = ts }

-- |
focusLeft :: Tree a -> Maybe (Tree a)
focusLeft x = do
  (l, ls) <- uncons (leading x)
  pure $ x { leading = ls, trailing = l : trailing x }

-- |
newtype History a = History
  { ancestry :: NonEmpty (Tree a)
  } deriving (Eq, Foldable, Functor, Show)

-- |
singleton :: a -> History a
singleton = History . pure . singletonTree

-- |
view :: History a -> a
view = content . NE.head . ancestry

-- |
record :: a -> History a -> History a
record a h = h { ancestry = singletonTree a <| ancestry h }

-- |
up :: History a -> Maybe (History a)
up h = do
  (a, bs) <- sequenceA . NE.uncons . ancestry $ h
  (b, cs) <- sequenceA . NE.uncons $ bs
  pure $ History (push a b <| cs)

-- |
down :: History a -> Maybe (History a)
down h = do
  (b, cs) <- sequenceA . NE.uncons . ancestry $ h
  (a, b') <- pop b
  pure $ History (a <| b' <| cs)

-- |
modifyParent :: (Tree a -> Maybe (Tree a)) -> History a -> Maybe (History a)
modifyParent f h = do
  u       <- up h
  (a, as) <- sequenceA . NE.uncons . ancestry $ u
  a'      <- f a
  down $ History (a' <| as)

-- |
right :: History a -> Maybe (History a)
right = modifyParent focusRight

-- |
left :: History a -> Maybe (History a)
left = modifyParent focusLeft
