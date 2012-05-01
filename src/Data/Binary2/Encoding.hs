{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Encoding2
-- Copyright   : 2012, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   :
-- Portability : portable
--
-- Binary encoding of values.
--
-----------------------------------------------------------------------------
module Data.Binary2.Encoding (

    -- * Streams of values to be encoded
      VStream
    , render
    , renderTextualUtf8

    -- ** Encoding combinators

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

    , integer
    , float
    , double

    , char

    , byteString
    
    , builder

    , (<>)

  ) where

import Prelude hiding (putChar)

import qualified Data.ByteString                                     as S
import qualified Data.ByteString.Char8                               as SC8
import qualified Data.ByteString.Lazy                                as L
import qualified Data.ByteString.Lazy.Builder                        as B
import qualified Data.ByteString.Lazy.Builder.ASCII                  as B
import qualified Data.ByteString.Lazy.Builder.Extras                 as B
import qualified Data.ByteString.Lazy.Builder.Internal               as B
import qualified Data.ByteString.Lazy.Builder.BasicEncoding          as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as E
import           Data.Monoid
import           Data.Word
import           Data.Int
import           Foreign.Ptr

infixr 6 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

------------------------------------------------------------------------

-- | The representation for a stream of values to be serialized.
data VStreamRep =
       VChar           {-# UNPACK #-} !Char         VStreamRep
     | VWord           {-# UNPACK #-} !Word         VStreamRep
     | VWord8          {-# UNPACK #-} !Word8        VStreamRep
     | VWord16         {-# UNPACK #-} !Word16       VStreamRep
     | VWord32         {-# UNPACK #-} !Word32       VStreamRep
     | VWord64         {-# UNPACK #-} !Word64       VStreamRep
     | VFloat          {-# UNPACK #-} !Float        VStreamRep
     | VDouble         {-# UNPACK #-} !Double       VStreamRep
     | VInteger                       !Integer      VStreamRep
     | VByteString                    !S.ByteString VStreamRep
     | VBuilder                       !B.Builder    VStreamRep
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

{-
-- | Binary encode a 'VStream' to a lazy bytestring 'B.Builder'.
render :: VStream -> B.Builder
render vs0 =
    go (toVStreamRep vs0 VEmpty)
  where
    go VEmpty                 = mempty
    go (VWord8  x vs)         = B.word8 x                   <> go vs
    go (VWord16 x vs)         = B.word16BE x                <> go vs
    go (VWord32 x vs)         = B.word32BE x                <> go vs
    go (VWord64 x vs)         = B.word64BE x                <> go vs
    go (VWord   x vs)         = B.word64BE (fromIntegral x) <> go vs
    go (VChar   x vs)         = B.charUtf8 x                <> go vs
    go (VFloat  x vs)         = B.floatBE  x                <> go vs
    go (VDouble x vs)         = B.doubleBE x                <> go vs
    go (VInteger x vs)        = error "render: integer: implement"
    go (VByteString x vs)     = B.byteString x              <> go vs
    go (VBuilder x vs)        = x                           <> go vs
-}

-- | Binary encode a 'VStream' to a lazy bytestring 'B.Builder'.
render :: VStream -> B.Builder
render fv0 =
    B.builder $ step (toVStreamRep fv0 VEmpty)
  where
    step fv1 k (B.BufferRange op0 ope0) = 
        go fv1 op0
      where
        go fv !op
          | op `plusPtr` bound <= ope0 = case fv of
              VEmpty            -> k (B.BufferRange op ope0)
              VWord8  x fv'     -> E.runB (E.fromF E.word8)    x                op >>= go fv'
              VWord16 x fv'     -> E.runB (E.fromF E.word16LE) x                op >>= go fv'
              VWord32 x fv'     -> E.runB (E.fromF E.word32LE) x                op >>= go fv'
              VWord64 x fv'     -> E.runB (E.fromF E.word64LE) x                op >>= go fv'
              VWord   x fv'     -> E.runB (E.fromF E.word64LE) (fromIntegral x) op >>= go fv'
              VChar   x fv'     -> E.runB E.charUtf8 x                          op >>= go fv'
              VFloat  x fv'     -> E.runB (E.fromF E.floatLE)  x                op >>= go fv'
              VDouble x fv'     -> E.runB (E.fromF E.doubleLE) x                op >>= go fv'
              VInteger _ _      -> error "render: integer: implement"
              VByteString x fv' -> B.runBuilderWith (B.byteString x) (step fv' k) (B.BufferRange op ope0)
              VBuilder x fv'    -> B.runBuilderWith x (step fv' k) (B.BufferRange op ope0)
          | otherwise = return $ B.bufferFull bound op (step fv k)

    bound = max (E.size E.word64LE) $ max (E.sizeBound E.charUtf8) (E.size E.doubleLE)

renderTextualUtf8 :: VStream -> L.ByteString
renderTextualUtf8 vs0 =
    B.toLazyByteString $ go (toVStreamRep vs0 VEmpty)
  where
    go VEmpty                   = mempty
    go (VWord8  1 (VChar x vs)) = line "w8,c 1," (B.charUtf8 x) vs
    go (VWord8  1 (VWord x vs)) = line "w8,w 1," (B.wordDec x) vs
    go (VWord l (VByteString x vs))
      | l > 0                   = line "w,bs " (B.wordDec l <> B.char8 ',' <> B.byteStringHexFixed x) vs
    go (VWord8  x vs)           = line "w8   " (B.word8Dec x)  vs
    go (VWord16 x vs)           = line "w16  " (B.word16Dec x) vs
    go (VWord32 x vs)           = line "w32  " (B.word32Dec x) vs
    go (VWord64 x vs)           = line "w64  " (B.word64Dec x) vs
    go (VWord   x vs)           = line "w    " (B.wordDec x)   vs
    go (VChar   x vs)           = line "c    " (B.charUtf8 x)  vs
    go (VFloat  x vs)           = line "f    " (B.floatDec x)  vs
    go (VDouble x vs)           = line "d    " (B.doubleDec x) vs
    go (VInteger x vs)          = line "I    " (B.integerDec x) vs
    go (VByteString x vs)       = line "bs   " (B.byteStringHexFixed x) vs
    go (VBuilder x vs)          = line "B    " (B.lazyByteStringHexFixed $ B.toLazyByteString x) vs

    line :: SC8.ByteString -> B.Builder -> VStreamRep -> B.Builder
    line pre b vs = B.byteStringCopy pre <> b <> B.char8 '\n' <> go vs


-- VStream construction
------------------------------

{-# INLINE float #-}
float :: Float -> VStream
float = VStream . VFloat

{-# INLINE double #-}
double :: Double -> VStream
double = VStream . VDouble

{-# INLINE integer #-}
integer :: Integer -> VStream
integer = VStream . VInteger

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
byteString = VStream . VByteString

{-# INLINE builder #-}
builder :: B.Builder -> VStream
builder = VStream . VBuilder




