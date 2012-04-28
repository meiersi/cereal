-----------------------------------------------------------------------------
-- |
-- Module      : Data.Serialize.EncodedValue
-- Copyright   : 2012, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   :
-- Portability :
--
-- Representation and renderings of a stream of binary encoded values.
--
-----------------------------------------------------------------------------
module Data.Serialize.FlatValue (

    -- * The Builder type
      FlatValueBuilder
    -- , toByteString
    , toLazyByteString

    -- * Constructing Builders

    -- , empty
    -- , singleton
    -- , append
    -- , fromByteString        -- :: S.ByteString -> Builder
    -- , fromLazyByteString    -- :: L.ByteString -> Builder

    -- * Flushing the buffer state
    -- , flush

    -- * Derived Builders
    -- ** Big-endian writes
    , putWord
    , putInt
    , putChar
    -- , putWord16be           -- :: Word16 -> Builder
    -- , putWord32be           -- :: Word32 -> Builder
    -- , putWord64be           -- :: Word64 -> Builder

    -- ** Little-endian writes
    -- , putWord16le           -- :: Word16 -> Builder
    -- , putWord32le           -- :: Word32 -> Builder
    -- , putWord64le           -- :: Word64 -> Builder

    -- ** Host-endian, unaligned writes
    -- , putWordhost           -- :: Word -> Builder
    -- , putWord16host         -- :: Word16 -> Builder
    -- , putWord32host         -- :: Word32 -> Builder
    -- , putWord64host         -- :: Word64 -> Builder

  ) where

import Prelude hiding (putChar)

import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Lazy.Builder as B
import           Data.Monoid
import           Data.Word

------------------------------------------------------------------------

data FlatValue =
       VEmpty
     | VWord    {-# UNPACK #-} !Word FlatValue
     | VChar    {-# UNPACK #-} !Char FlatValue
     | VBuilder B.Builder            FlatValue

newtype FlatValueBuilder = FlatValueBuilder { toFlatValue :: FlatValue -> FlatValue }

instance Monoid FlatValueBuilder where
  {-# INLINE mempty #-}
  mempty = FlatValueBuilder id

  {-# INLINE mappend #-}
  b1 `mappend` b2 = FlatValueBuilder (toFlatValue b1 . toFlatValue b2)

toLazyByteString :: FlatValueBuilder -> L.ByteString
toLazyByteString fvb = B.toLazyByteString $ toBuilder $ toFlatValue fvb VEmpty

{-# INLINE putInt #-}
putInt :: Int -> FlatValueBuilder
putInt = putWord . fromIntegral

{-# INLINE putWord #-}
putWord :: Word -> FlatValueBuilder
putWord = FlatValueBuilder . VWord

{-# INLINE putChar #-}
putChar :: Char -> FlatValueBuilder
putChar = FlatValueBuilder . VChar


toBuilder :: FlatValue -> B.Builder
toBuilder VEmpty          = mempty
toBuilder (VWord w fv)    = B.word64BE (fromIntegral w) `mappend` toBuilder fv
toBuilder (VChar c fv)    = B.charUtf8 c                `mappend` toBuilder fv
toBuilder (VBuilder b fv) = b                           `mappend` toBuilder fv


{-

-- NESTED
---------

data FlatValue =
       VEmpty
     | VAppend FlatValue FlatValue
     | VWord    {-# UNPACK #-} !Word
     | VChar    {-# UNPACK #-} !Char
     | VBuilder B.Builder


type FlatValueBuilder = FlatValue

instance Monoid FlatValue where
  {-# INLINE mempty #-}
  mempty = VEmpty

  {-# INLINE mappend #-}
  mappend = VAppend

  {-# INLINE mconcat #-}
  mconcat = foldr VAppend VEmpty

toLazyByteString :: FlatValueBuilder -> L.ByteString
toLazyByteString = B.toLazyByteString . toBuilder

{-# INLINE putInt #-}
putInt :: Int -> FlatValueBuilder
putInt = putWord . fromIntegral

{-# INLINE putWord #-}
putWord :: Word -> FlatValueBuilder
putWord = VWord

{-# INLINE putChar #-}
putChar :: Char -> FlatValueBuilder
putChar = VChar


toBuilder :: FlatValue -> B.Builder
toBuilder VEmpty            = mempty
toBuilder (VAppend fv1 fv2) = toBuilder fv1 `mappend` toBuilder fv2
toBuilder (VWord w)         = B.word64BE (fromIntegral w)
toBuilder (VChar c)         = B.charUtf8 c
toBuilder (VBuilder b)      = b

-}
