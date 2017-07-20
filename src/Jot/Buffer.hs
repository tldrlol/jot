module Jot.Buffer
  ( Buffer(..)
  , load
  ) where

import           Control.Concurrent.Async
import qualified Data.ByteString          as BS

import qualified Jot.Edit    as E
import qualified Jot.History as H
import qualified Jot.Range   as R
import qualified Jot.Rope    as S

-- |
newtype Buffer = Buffer
  { edits :: H.History (R.Range, S.JotString)
  }

-- |
load :: FilePath -> IO (Async Buffer)
load
  = async
  . fmap (Buffer . E.load . S.singleton)
  . BS.readFile
