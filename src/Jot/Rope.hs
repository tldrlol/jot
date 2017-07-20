{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Jot.Rope
  ( JotString
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
  , slice
  , splice
  , copy
  , delete
  , reverse
  ) where

import           Control.Arrow         ((&&&))
import           Control.Lens          (_Just, _2, preview, to)
import           Control.Monad         (guard)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as BL
import qualified Data.FingerTree       as T
import           Data.FingerTree       (FingerTree, Measured(..), ViewL(..), ViewR(..))
import           Data.Foldable         (foldl')
import           Data.Function         (on)
import           Data.Semigroup        (Semigroup(..))
import           Prelude               hiding (concat, length, null, replicate, reverse, splitAt)

import           Jot.Range (Range(..), size)

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
    (EmptyR, _)          -> JotString t
    (_, EmptyL)          -> JotString s
    (ss :> m1, m2 :< ts) -> JotString $ ss `mappend` lump m1 m2 `mappend` ts
  where
    lump c1 c2 =
      if ((+) `on` length') c1 c2 <= nominalChunkSize 
        then T.singleton (c1 <> c2)
        else (mappend `on` T.singleton) c1 c2

-- |
concat :: Foldable t => t JotString -> JotString
concat = foldl' (<>) mempty

-- |
replicate :: Int -> JotString -> JotString
replicate n s
  | n <= 0    = mempty
  | even n    = t <> t
  | otherwise = t <> t <> s
  where
    t = replicate (n `div` 2) s

-- |
splitAt :: Int -> JotString -> Maybe (JotString, JotString)
splitAt n (JotString s) = do
  guard $ 0 <= n && n <= length' s
  let (l, r) = T.split (\m -> len m > n) s
  pure $ case T.viewl r of
    EmptyL           -> (JotString l, JotString r)
    JotChunk x :< xs ->
      let (m1, m2) = BS.splitAt (n - len (measure l)) x
      in  (JotString l <> singleton m1, singleton m2 <> JotString xs)

-- |
splitRange :: Range -> JotString -> Maybe (JotString, JotString, JotString)
splitRange r@(Range i _) s = do
  let sz = size r
  guard $ sz >= 0
  (a, t) <- splitAt i  s
  (b, c) <- splitAt sz t
  pure (a, b, c)

-- |
slice :: Range -> JotString -> Maybe JotString
slice r = preview $ to (splitRange r) . _Just._2

-- |
splice :: JotString -> Range -> JotString -> Maybe JotString
splice t r s = do
  (pr, _, sf) <- splitRange r s
  pure $ pr <> t <> sf

-- |
copy :: Range -> Range -> JotString -> Maybe JotString
copy src dst s = do
  t <- slice src s
  splice t dst s

-- |
delete :: Range -> JotString -> Maybe JotString
delete = splice mempty

-- |
reverse :: JotString -> JotString
reverse
  = JotString
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
