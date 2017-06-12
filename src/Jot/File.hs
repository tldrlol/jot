module Jot.File
  ( readFile
  ) where

import qualified Control.Concurrent.Async as A
import qualified Data.ByteString          as BS
import           Prelude                  hiding (readFile)

import qualified Jot.Buffer as B

-- |
readFile :: FilePath -> IO (A.Async B.Buffer)
readFile p = do
  c <- A.async (BS.readFile p)
  pure $ B.buffer (Just p) <$> c
