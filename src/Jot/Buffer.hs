{-# LANGUAGE BangPatterns #-}

module Jot.Buffer
  ( Buffer(..)
  , load
  ) where

import           Control.Concurrent.Async
import qualified Data.ByteString.Lazy     as BL

import qualified Jot.Edit    as E
import qualified Jot.History as H
import qualified Jot.Range   as R
import qualified Jot.Rope    as S

-- |
newtype Buffer = Buffer
  { edits :: H.History (R.Range, S.JotString)
  } deriving (Show)

-- |
forceAsync :: a -> IO (Async a)
forceAsync a = do
  async $ do
    pure ()
    let !a' = a
    pure a'

-- |
load :: FilePath -> IO (Async Buffer)
load p = async $ do
  f <- BL.readFile p
  let  s = S.concat $ S.singleton <$> BL.toChunks f
  let !_ = S.length s
  _ <- forceAsync (S.newLines s)
  pure $ Buffer $ E.load s
