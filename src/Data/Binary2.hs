{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary2
-- Copyright   : 2012, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   :
-- Portability :
--
-----------------------------------------------------------------------------

module Data.Binary2 (

    -- * The Binary2 class
      Binary2(..)
    , toLazyByteString

    ) where

import Data.Binary2.Encoding
-- import Data.Binary2.Get

import Data.Word
import Data.Monoid
import Data.Foldable (foldMap)
import Foreign

-- And needed for the instances:
import           Data.Array.Unboxed
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L (foldrChunks)
import qualified Data.ByteString.Lazy.Builder  as B
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.IntMap                   as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Ratio                    as R
import qualified Data.Tree                     as T
import qualified Data.Sequence                 as Seq


------------------------------------------------------------------------

type Encoding t = t -> VStream

-- | If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@), the 'encode' and 'get'
-- methods will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause to your datatype
-- and declare a 'Binary2' instance for it without giving a definition for
-- 'encode' and 'get'.
class Binary2 t where
    -- | Encode a value in the Put monad.
    encode :: Encoding t

-- | Encode a value to a lazy 'L.ByteString'.
toLazyByteString :: Binary2 t => t -> L.ByteString
toLazyByteString = B.toLazyByteString . render . encode

------------------------------------------------------------------------
-- Simple instances

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Binary2 () where
    {-# INLINE encode #-}
    encode ()  = mempty

-- Bools are encoded as a byte in the range 0 .. 1
instance Binary2 Bool where
    {-# INLINE encode #-}
    encode     = word8 . fromIntegral . fromEnum

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Binary2 Ordering where
    {-# INLINE encode #-}
    encode     = word8 . fromIntegral . fromEnum

------------------------------------------------------------------------
-- Words and Ints

-- Words8s are written as bytes
instance Binary2 Word8 where
    {-# INLINE encode #-}
    encode     = word8

-- Words16s are written as 2 bytes in big-endian (network) order
instance Binary2 Word16 where
    {-# INLINE encode #-}
    encode     = word16

-- Words32s are written as 4 bytes in big-endian (network) order
instance Binary2 Word32 where
    {-# INLINE encode #-}
    encode     = word32

-- Words64s are written as 8 bytes in big-endian (network) order
instance Binary2 Word64 where
    {-# INLINE encode #-}
    encode     = word64

-- Int8s are written as a single byte.
instance Binary2 Int8 where
    {-# INLINE encode #-}
    encode     = int8

-- Int16s are written as a 2 bytes in big endian format
instance Binary2 Int16 where
    {-# INLINE encode #-}
    encode     = int16

-- Int32s are written as a 4 bytes in big endian format
instance Binary2 Int32 where
    {-# INLINE encode #-}
    encode     = int32

-- Int64s are written as a 8 bytes in big endian format
instance Binary2 Int64 where
    {-# INLINE encode #-}
    encode     = int64

------------------------------------------------------------------------

-- Words are are written as Word64s, that is, 8 bytes in big endian format
instance Binary2 Word where
    {-# INLINE encode #-}
    encode   = word

-- Ints are are written as Int64s, that is, 8 bytes in big endian format
instance Binary2 Int where
    {-# INLINE encode #-}
    encode  = int

instance Binary2 Integer where
    {-# INLINE encode #-}
    encode = integer

instance (Binary2 a, Integral a) => Binary2 (R.Ratio a) where
    {-# INLINE encode #-}
    encode = \r -> encode (R.numerator r) <> encode (R.denominator r)

instance Binary2 Char where
    {-# INLINE encode #-}
    encode = char

instance (Binary2 a, Binary2 b) => Binary2 (a,b) where
    {-# INLINE encode #-}
    encode (a,b) = encode a <> encode b

instance (Binary2 a, Binary2 b, Binary2 c) => Binary2 (a,b,c) where
    {-# INLINE encode #-}
    encode (a,b,c) = encode a <> encode b <> encode c

instance (Binary2 a, Binary2 b, Binary2 c, Binary2 d)
        => Binary2 (a,b,c,d) where
    encode (a,b,c,d) = encode a <> encode b <> encode c <> encode d

instance (Binary2 a, Binary2 b, Binary2 c, Binary2 d, Binary2 e)
        => Binary2 (a,b,c,d,e) where
    encode (a,b,c,d,e) = encode a <> encode b <> encode c <> encode d <> encode e

-- 
-- and now just recurse:
--

instance (Binary2 a, Binary2 b, Binary2 c, Binary2 d, Binary2 e
         , Binary2 f)
        => Binary2 (a,b,c,d,e,f) where
    encode (a,b,c,d,e,f) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f
 

instance (Binary2 a, Binary2 b, Binary2 c, Binary2 d, Binary2 e
         , Binary2 f, Binary2 g)
        => Binary2 (a,b,c,d,e,f,g) where
    encode (a,b,c,d,e,f,g) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g

instance (Binary2 a, Binary2 b, Binary2 c, Binary2 d, Binary2 e,
          Binary2 f, Binary2 g, Binary2 h)
        => Binary2 (a,b,c,d,e,f,g,h) where
    encode (a,b,c,d,e,f,g,h) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h

instance (Binary2 a, Binary2 b, Binary2 c, Binary2 d, Binary2 e,
          Binary2 f, Binary2 g, Binary2 h, Binary2 i)
        => Binary2 (a,b,c,d,e,f,g,h,i) where
    encode (a,b,c,d,e,f,g,h,i) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h <> encode i

instance (Binary2 a, Binary2 b, Binary2 c, Binary2 d, Binary2 e,
          Binary2 f, Binary2 g, Binary2 h, Binary2 i, Binary2 j)
        => Binary2 (a,b,c,d,e,f,g,h,i,j) where
    encode (a,b,c,d,e,f,g,h,i,j) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h <> encode i <> encode j

------------------------------------------------------------------------
-- Container types

-- | Share list encoding, as it is required for faster tree encoding.
{-# INLINE encodeList #-}
encodeList :: Encoding a -> Encoding [a]
encodeList f = (<> word8 0) . foldMap ((word8 1 <>) . f)

instance Binary2 a => Binary2 [a] where
    {-# INLINE encode #-}
    encode = encodeList encode

instance (Binary2 a) => Binary2 (Maybe a) where
    {-# INLINE encode #-}
    encode = maybe (word8 0) ((word8 1 <>) . encode)

instance (Binary2 a, Binary2 b) => Binary2 (Either a b) where
    {-# INLINE encode #-}
    encode = either ((word8 0 <>) . encode) ((word8 1 <>) . encode)

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Binary2 S.ByteString where
    {-# INLINE encode #-}
    encode = \bs -> int (S.length bs) <> byteString bs

instance Binary2 L.ByteString where
    encode = (<> int 0) . L.foldrChunks (\bs s -> encode bs <> s) mempty

------------------------------------------------------------------------
-- Maps and Sets

instance (Ord a, Binary2 a) => Binary2 (Set.Set a) where
    {-# INLINE encode #-}
    encode = encode . Set.toAscList

instance (Ord k, Binary2 k, Binary2 e) => Binary2 (Map.Map k e) where
    {-# INLINE encode #-}
    encode = encode . Map.toAscList

instance Binary2 IntSet.IntSet where
    {-# INLINE encode #-}
    encode = encode . IntSet.toAscList

instance (Binary2 e) => Binary2 (IntMap.IntMap e) where
    {-# INLINE encode #-}
    encode = encode . IntMap.toAscList

------------------------------------------------------------------------
-- Queues and Sequences

instance (Binary2 e) => Binary2 (Seq.Seq e) where
    {-# INLINE encode #-}
    encode = \s -> int (Seq.length s) <> foldMap encode s

------------------------------------------------------------------------
-- Floating point

instance Binary2 Double where
    {-# INLINE encode #-}
    encode = double

instance Binary2 Float where
    {-# INLINE encode #-}
    encode = float

------------------------------------------------------------------------
-- Trees

instance (Binary2 e) => Binary2 (T.Tree e) where
    {-# INLINE encode #-}
    encode =
        go
      where 
        go (T.Node x cs) = encode x <> encodeList go cs

------------------------------------------------------------------------
-- Arrays

instance (Binary2 i, Ix i, Binary2 e) => Binary2 (Array i e) where
    {-# INLINE encode #-}
    encode = \a -> encode (bounds a) <> encode (elems a)
--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (Binary2 i, Ix i, Binary2 e, IArray UArray e) => Binary2 (UArray i e) where
    {-# INLINE encode #-}
    encode = \a -> encode (bounds a) <> encode (elems a)

