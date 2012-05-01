{-# LANGUAGE GADTs, PackageImports, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark encoding and decoding speed.
module Main (main) where

import           Prelude hiding (words)
import           Criterion.Main

import           Data.Binary2.Encoding (renderTextualUtf8, renderTagged)
import qualified Data.ByteString             as S
import qualified Data.ByteString.Lazy        as L
import qualified Data.ByteString.Lazy.Char8  as LC8
import           Data.Serialize
import qualified Data.Serialize2 as S2
import           Data.Monoid

import           Data.Binary (Binary)
import qualified Data.Binary as Binary

import qualified Data.Binary2 as B2

import qualified Data.Sequence as Seq
import           Data.Tree


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- | The number of repetitions to consider.
nRepl :: Int
nRepl = 1000

-- We use NOINLINE to ensure that GHC has no chance of optimizing too much.

{-# NOINLINE intData #-}
intData :: Int -> [Int]
intData n = take n [0..]

{-# NOINLINE stringData #-}
stringData :: Int -> [String]
stringData n = take n $ cycle ["hello", "world"]

{-# NOINLINE seqIntData #-}
seqIntData :: Int -> Seq.Seq Int
seqIntData = Seq.fromList . intData

-- | Build a balanced binary tree.
{-# NOINLINE treeIntData #-}
treeIntData :: Int -> Tree Int
treeIntData n = 
   head $ go [0..n]  -- assuming n >= 0
  where
   go []  = []
   go [x] = [Node x []]
   go xs  =
       [Node r $ concatMap go [ls, rs]]
     where
       (ls, r:rs) = splitAt (length xs `div` 2) xs

testValue n = replicate n $ Just 
    ("Haskell", S.pack [0xbe,0xef], [-2..1::Int], 0.123 :: Double)

-- benchmarks
-------------

main :: IO ()
main = Criterion.Main.defaultMain $ 
    [ benchmarks "testValue memoized "  id         (testValue nRepl)
    , benchmarks "Tree Int memoized "  id         (treeIntData nRepl)
    , benchmarks "Seq Int memoized "   id         (seqIntData nRepl)
    , benchmarks "[Int] memoized "     id         (intData nRepl)
    -- , benchmarks "[Int] generated "    intData    nRepl
    , benchmarks "[String] memoized"   id         (stringData nRepl)
    -- , benchmarks "[String] generated"  stringData nRepl
    ]
  where
    benchmarks :: (Binary a, B2.Binary2 a, S2.Serialize a, Serialize a) 
               => String -> (b -> a) -> b -> Benchmark
    benchmarks name f x = bgroup (name ++ show nRepl)
      [ bench "new binary" $ whnf (L.length . B2.toLazyByteString . f) x
      , bench "new binary tagged" $ whnf (L.length . renderTagged . B2.encode . f) x
      , bench "cereal" $ whnf (L.length . encodeLazy . f)  x
      , bench "value stream" $ whnf (L.length . S2.encodeLazy . f)  x
      , bench "binary" $ whnf (L.length . Binary.encode . f) x
      ]

-- | Testing the new binary encoding format.
testNewBinary :: B2.Binary2 a => a -> IO ()
testNewBinary x = 
  LC8.putStrLn $ renderTextualUtf8 $ B2.encode (x, B2.toLazyByteString x)


