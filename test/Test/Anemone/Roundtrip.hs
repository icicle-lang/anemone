{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Anemone.Roundtrip where

import           Anemone.Parser
import           Anemone.Pretty

import           Control.Monad.ST (runST, ST)

import           Data.Array.ST (MArray, STUArray, newArray, readArray)
import           Data.Array.Unsafe (castSTUArray)
import qualified Data.ByteString.Char8 as Char8

import           Hedgehog
import           Hedgehog.Gen.QuickCheck (arbitrary)

import           P

import           System.IO (IO)


prop_roundtrip_double :: Property
prop_roundtrip_double = property $ do
  (n :: Int64)  <- forAll arbitrary
  (x :: Double) <- forAll arbitrary
  let
    original =
      0 + x * fromIntegral n

    intermediate =
      renderDouble original

    Just (roundtrip, "") =
      parseDouble intermediate

    original_i64 =
      fromDouble original

    roundtrip_i64 =
      fromDouble roundtrip

    diff_i64 =
      abs (original_i64 - roundtrip_i64)

  annotate $
    concat
      [ ""
      , "Roundtrip failed."
      , ""
      , "=== Original ==="
      , (show original)
      , ""
      , "=== Intermediate ==="
      , (Char8.unpack intermediate)
      , ""
      , "=== Roundtrip ==="
      , (show roundtrip)
      , ""
      , "=== Original (Int64) ==="
      , (show original_i64)
      , ""
      , "=== Roundtrip (Int64) ==="
      , (show roundtrip_i64)
      , ""
      , "=== Diff (Int64) ==="
      , (show diff_i64)
      ]
  assert $
    diff_i64 <= 1

fromDouble :: Double -> Int64
fromDouble x =
  runST (cast x)

cast ::
     MArray (STUArray s) a (ST s)
  => MArray (STUArray s) b (ST s)
  => a
  -> ST s b
cast x =
   flip readArray 0 =<< castSTUArray =<< newArray (0 :: Int, 0) x
{-# INLINE cast #-}

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)