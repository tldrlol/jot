{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Jot.Rope
  ( JotString
  , Range
  , null
  , length
  , newLines
  , singleton
  , toLazyByteString
  , empty
  , append
  , concat
  , replicate
  , splitAt
  , splice
  , reverse
  ) where

import           Control.Arrow         ((&&&))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as BL
import           Data.FingerTree       (FingerTree, Measured (..), ViewL (..), ViewR (..))
import qualified Data.FingerTree       as T
import           Data.Foldable         (foldl')
import           Data.Function         (on)
import           Data.Semigroup        (Semigroup (..))
import           Prelude               hiding (concat, length, null, replicate, reverse, splitAt)

-- |
newtype JotString = JotString
  { fromRope :: FingerTree JotSize JotChunk
  } deriving (Show)

-- |
data JotSize = JotSize
  { len :: {-# UNPACK #-} !Int
  , nls ::                 Int
  } deriving (Show)

-- |
newtype JotChunk = JotChunk
  { fromChunk :: BS.ByteString
  } deriving (Semigroup, Show)

-- |
data Range = Range
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  deriving Show

-- |
null :: JotString -> Bool
null = T.null . fromRope

-- |
length' :: Measured JotSize m => m -> Int
length' = len . measure

-- |
length :: JotString -> Int
length = length'

-- |
newLines :: JotString -> Int
newLines = nls . measure

-- |
singleton :: BS.ByteString -> JotString
singleton bs
  | BS.null bs = mempty
  | otherwise  = JotString (T.singleton (JotChunk bs))

-- |
toLazyByteString :: JotString -> BL.ByteString
toLazyByteString = foldMap (BL.fromStrict . fromChunk) . fromRope

-- |
nominalChunkSize :: Int
nominalChunkSize = 512

-- |
empty :: JotString
empty = JotString mempty

-- |
append :: JotString -> JotString -> JotString
append (JotString s) (JotString t) =
  case (T.viewr s, T.viewl t) of
    (EmptyR, _) -> JotString t
    (_, EmptyL) -> JotString s
    (ss :> m1, m2 :< ts) ->
      JotString $ ss `mappend` lump m1 m2 `mappend` ts
  where
    lump c1 c2
      | ((+) `on` length') c1 c2 <= nominalChunkSize = T.singleton (c1 <> c2)
      | otherwise = (mappend `on` T.singleton) c1 c2

-- |
concat :: Foldable t => t JotString -> JotString
concat = foldl' (<>) mempty

-- |
replicate :: Int -> JotString -> JotString
replicate 0 _ = mempty
replicate n s
  | even n    = t <> t
  | otherwise = t <> t <> s
  where
    t = replicate (n `div` 2) s

-- |
splitAt :: Int -> JotString -> (JotString, JotString)
splitAt n (JotString s)
  | n <= 0 = (mempty, JotString s)
  | otherwise =
    let (l, r) = T.split ((> n) . len) s
    in  case T.viewl r of
      EmptyL           -> (JotString l, JotString r)
      JotChunk x :< xs ->
        let (m1, m2) = BS.splitAt (n - len (measure l)) x
        in  (JotString l <> singleton m1, singleton m2 <> JotString xs)

-- |
splice :: Range -> JotString -> JotString -> JotString
splice (Range i j) t s = pr <> t <> sf
  where
    (pr, _) = splitAt i s
    (_, sf) = splitAt j s

-- |
reverse :: JotString -> JotString
reverse = JotString
        . T.reverse
        . T.unsafeFmap (JotChunk . BS.reverse . fromChunk)
        . fromRope

instance Monoid JotSize where
  mempty = JotSize 0 0
  mappend (JotSize l n) (JotSize l' n') = JotSize (l+l') (n+n')

instance Semigroup JotSize

instance Measured JotSize JotChunk where
  measure = (JotSize <$> BS.length <*> C.count '\n') . fromChunk

instance Measured JotSize JotString where
  measure = measure . fromRope

instance Monoid JotString where
  mempty  = empty
  mappend = append
  mconcat = concat

instance Semigroup JotString

instance Eq JotString where
  (==) = (==) `on` (length' &&& toLazyByteString)
