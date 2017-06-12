module Jot.Buffer
  ( Buffer
  , buffer
  , empty
  , length
  , slice
  , toJotString
  , redo
  , edit
  , copy
  , del
  , undo
 ) where

import qualified Data.ByteString as BS
import           Prelude         hiding (length)

import qualified Jot.History as H
import qualified Jot.Rope    as R
import           Jot.Range

-- |
data Buffer = Buffer
  { history  :: H.History R.JotString
  , metadata :: Metadata
  } deriving (Show)

-- |
data Metadata = Metadata
  { filePath :: Maybe FilePath
  } deriving (Show)

-- |
buffer :: (Maybe FilePath) -> BS.ByteString -> Buffer
buffer p c =
  Buffer
    { history = H.singleton . R.singleton $ c
    , metadata =
        Metadata
          { filePath = p
          }
    }

-- |
empty :: Buffer
empty = buffer Nothing mempty

-- |
length :: Buffer -> Int
length = R.length . H.view . history

-- |
slice :: Range -> Buffer -> Maybe R.JotString
slice r b = R.slice r (H.view (history b))

-- |
toJotString :: Buffer -> R.JotString
toJotString = H.view . history

-- |
edit :: Range -> R.JotString -> Buffer -> Maybe Buffer
edit r s b = do
  s' <- R.edit r s  . H.view . history $ b
  pure $ b { history = H.record s' (history b) }

-- |
copy :: Range -> Range -> Buffer -> Maybe Buffer
copy r t b = do
  s <- R.slice r . H.view . history $ b
  edit t s b

-- |
del :: Range -> Buffer -> Maybe Buffer
del = flip edit mempty

-- |
undo :: Buffer -> Maybe Buffer
undo b = do
  h <- H.up . history $ b
  pure $ b { history = h }

-- |
redo :: Buffer -> Maybe Buffer
redo b = do
  h <- H.down . history $ b
  pure $ b { history = h }
