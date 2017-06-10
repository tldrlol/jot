{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Jot.History
  ( History
  , empty
  , singleton
  , view
  , record
  , undo
  , redo
  , next
  , prev
  ) where

import Control.Monad((>=>))
import Data.List (uncons)
import Safe (headMay)

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
focusNext :: Tree a -> Maybe (Tree a)
focusNext x = do
  (t, ts) <-  uncons (trailing x)
  pure $ x { leading = t : leading x, trailing = ts }

-- |
focusPrev :: Tree a -> Maybe (Tree a)
focusPrev x = do
  (l, ls) <- uncons (leading x)
  pure $ x { leading = ls, trailing = l : trailing x }

-- |
newtype History a = History
  { ancestry :: [Tree a]
  } deriving (Eq, Foldable, Functor, Show)

-- |
empty :: History a
empty = History []

-- |
singleton :: a -> History a
singleton = History . pure . singletonTree

-- |
view :: History a -> Maybe a
view = (headMay >=> pure . content) . ancestry

-- |
record :: a -> History a -> History a
record a h = h { ancestry = singletonTree a : ancestry h }

-- |
undo :: History a -> Maybe (History a)
undo h = do
  (a, bs) <- uncons . ancestry $ h
  (b, cs) <- uncons bs
  pure $ History (push a b : cs)

-- |
redo :: History a -> Maybe (History a)
redo h = do
  (b, cs) <- uncons . ancestry $ h
  (a, b') <- pop b
  pure $ History (a : b' : cs)

-- |
modifyParent :: (Tree a -> Maybe (Tree a)) -> History a -> Maybe (History a)
modifyParent f h = do
  u       <- undo h
  (a, as) <- uncons . ancestry $ u
  a'      <- f a
  redo $ History (a' : as)

-- |
next :: History a -> Maybe (History a)
next = modifyParent focusNext

-- |
prev :: History a -> Maybe (History a)
prev = modifyParent focusPrev
