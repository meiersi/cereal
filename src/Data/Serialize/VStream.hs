{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Serialize.VStream
-- Copyright   : 2012, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   :
-- Portability : portable
--
-- Representation and renderings of a stream of values that should be binary
-- encoded.
--
-----------------------------------------------------------------------------
module Data.Serialize.VStream (

    -- * Streams of values to be encoded
      VStream
    , encode

    -- ** Construction
    , word
    , word8
    , word16
    , word32
    , word64

    , int
    , int8
    , int16
    , int32
    , int64

    , char

    , byteString
    , lazyByteString

    , builder

  ) where

import Prelude hiding (putChar)

import qualified Data.ByteString                            as S
import qualified Data.ByteString.Lazy                       as L
import qualified Data.Serialize.Builder                     as B
import           Data.Monoid
import           Data.Word
import           Data.Int
-- import           Foreign.Ptr

------------------------------------------------------------------------

-- | The representation for a stream of values to be serialized.
data VStreamRep =
       VChar           {-# UNPACK #-} !Char         VStreamRep
     | VWord           {-# UNPACK #-} !Word         VStreamRep
     | VWord8          {-# UNPACK #-} !Word8        VStreamRep
     | VWord16         {-# UNPACK #-} !Word16       VStreamRep
     | VWord32         {-# UNPACK #-} !Word32       VStreamRep
     | VWord64         {-# UNPACK #-} !Word64       VStreamRep
     | VByteString                    !S.ByteString VStreamRep
     | VLazyByteString                !L.ByteString VStreamRep
     | VBuilder                       B.Builder     VStreamRep
     | VEmpty

-- | A stream of values to be encoded.
newtype VStream = VStream { toVStreamRep :: VStreamRep -> VStreamRep }

instance Monoid VStream where
  {-# INLINE mempty #-}
  mempty                 = VStream id
  {-# INLINE mappend #-}
  b1 `mappend` b2        = VStream (toVStreamRep b1 . toVStreamRep b2)
  {-# INLINE mconcat #-}
  mconcat                = foldr mappend mempty

-- | Binary encode a 'VStream' to a lazy bytestring 'B.Builder'.
encode :: VStream -> B.Builder
encode vs0 =
    go (toVStreamRep vs0 VEmpty)
  where
    ap = mappend
    
    go VEmpty                 = mempty
    go (VWord8  x vs)         = B.singleton x                  `ap` go vs
    go (VWord16 x vs)         = B.putWord16be x                `ap` go vs
    go (VWord32 x vs)         = B.putWord32be x                `ap` go vs
    go (VWord64 x vs)         = B.putWord64be x                `ap` go vs
    go (VWord   x vs)         = B.putWord64be (fromIntegral x) `ap` go vs
    go (VChar   x vs)         = B.putCharUtf8 x                `ap` go vs
    go (VByteString x vs)     = B.fromByteString x             `ap` go vs
    go (VLazyByteString x vs) = B.fromLazyByteString x         `ap` go vs
    go (VBuilder x vs)        = x                              `ap` go vs


-- VStream construction
------------------------------

{-# INLINE word #-}
word :: Word -> VStream
word = VStream . VWord

{-# INLINE word8 #-}
word8 :: Word8 -> VStream
word8 = VStream . VWord8

{-# INLINE word16 #-}
word16 :: Word16 -> VStream
word16 = VStream . VWord16

{-# INLINE word32 #-}
word32 :: Word32 -> VStream
word32 = VStream . VWord32

{-# INLINE word64 #-}
word64 :: Word64 -> VStream
word64 = VStream . VWord64

{-# INLINE int #-}
int :: Int -> VStream
int = word . fromIntegral

{-# INLINE int8 #-}
int8 :: Int8 -> VStream
int8 = word8 . fromIntegral

{-# INLINE int16 #-}
int16 :: Int16 -> VStream
int16 = word16 . fromIntegral

{-# INLINE int32 #-}
int32 :: Int32 -> VStream
int32 = word32 . fromIntegral

{-# INLINE int64 #-}
int64 :: Int64 -> VStream
int64 = word64 . fromIntegral

{-# INLINE char #-}
char :: Char -> VStream
char = VStream . VChar

{-# INLINE byteString #-}
byteString :: S.ByteString -> VStream
byteString = VStream . VBuilder . B.fromByteString

{-# INLINE lazyByteString #-}
lazyByteString :: L.ByteString -> VStream
lazyByteString = VStream . VBuilder . B.fromLazyByteString
-- lazyByteString = VStream . VLazyByteString

{-# INLINE builder #-}
builder :: B.Builder -> VStream
builder = VStream . VBuilder
