{-# LANGUAGE TupleSections #-}

module Jot.Edit
  ( load
  , edit
  , splice
  , copy
  , delete
  ) where

import           Control.Lens

import qualified Jot.History as H
import qualified Jot.Range   as R
import qualified Jot.Rope    as S

-- |
load
  :: S.JotString
  -> H.History (R.Range, S.JotString)
load =
  fmap (R.ofPoint 0 ,) . H.singleton

edit
  :: Monad m
  => (r -> s -> m s)
  -> r
  -> H.History (r, s)
  -> m (H.History (r, s))
edit f r h = do
  let s = h ^. to H.study . _2
  s' <- f r s
  pure $ H.record (r, s') h

-- |
splice
  :: S.JotString
  -> R.Range
  -> H.History (R.Range, S.JotString)
  -> Maybe (H.History (R.Range, S.JotString))
splice t = edit (S.splice t)

-- |
copy
  :: R.Range
  -> R.Range
  -> H.History (R.Range, S.JotString)
  -> Maybe (H.History (R.Range, S.JotString))
copy src = edit (S.copy src) 

-- |
delete
  :: R.Range
  -> H.History (R.Range, S.JotString)
  -> Maybe (H.History (R.Range, S.JotString))
delete = edit S.delete 
