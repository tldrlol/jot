{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Jot.History
  ( History
  , singleton
  , study
  , record
  , undo
  , redo
  , focusNext
  , focusPrev
  ) where

import           Control.Lens
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup (Semigroup, (<>))

-- |
data Tree a = Tree
  { content   :: a
  , _leading  :: [Tree a]
  , _trailing :: [Tree a]
  } deriving (Functor, Show)

makeLenses ''Tree

-- |
newtype History a = History
  { archives :: NonEmpty (Tree a)
  } deriving (Functor, Semigroup, Show)

-- |
singletonTree :: a -> Tree a
singletonTree a = Tree a [] []

-- |
singleton :: a -> History a
singleton = History . pure . singletonTree

-- |
study :: History a -> a
study = content . NE.head  . archives

-- |
record :: a -> History a -> History a
record = (<>) . singleton

-- |
undo :: History a -> Maybe (History a)
undo (History (a :| b:ts)) = Just $ History $ (b & trailing %~ cons a) :| ts
undo _                  = Nothing

-- |
redo :: History a -> Maybe (History a)
redo (History (t :| ts)) = do
  (c, cs) <- uncons (t ^. trailing)
  Just $ History $ c :| (t & trailing .~ cs) : ts

-- |
focus
  :: Lens' (Tree a) [Tree a]
  -> Lens' (Tree a) [Tree a]
  -> History a
  -> Maybe (History a)
focus src dst (History (t :| ts)) = do
  (c, cs) <- uncons (t ^. src)
  Just $ History $ (t & src .~ cs & dst %~ cons c) :| ts

-- |
focusNext :: History a -> Maybe (History a)
focusNext = focus trailing leading

-- |
focusPrev :: History a -> Maybe (History a)
focusPrev = focus leading trailing
