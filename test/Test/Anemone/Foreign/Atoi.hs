{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Atoi where

import qualified  Anemone.Foreign.Atoi as Atoi

import            P
import            Test.QuickCheck
import            Test.QuickCheck.Instances()

import qualified  Data.ByteString       as B
import qualified  Data.ByteString.Char8 as BC

import Data.Char (isDigit)
import Data.List (replicate)


testAtoi  :: Atoi.AtoiT
          -> B.ByteString -> Property
testAtoi check a
 = let r  = readInt64 (BC.unpack a)
   in  (r === check a)
 where
  readInt64 xs = do
    full      :: Integer <- readMaybe' xs
    let f64   :: Int64    = fromIntegral full
    let full' :: Integer  = fromIntegral f64
    if full == full' then return f64 else Nothing

  -- Our atoi does not remove leading whitespace
  readMaybe' ('-':xs)
   = fmap negate $ readDigits 0 xs
  readMaybe' xs
   = readDigits 0 xs

  readDigits i (x:xs)
   | isDigit x
   , Just d <- readMaybe [x]
   = let i' = i * 10 + d
     in  case readDigits i' xs of
          Nothing -> Just i'
          Just d' -> Just d'
   | otherwise
   = Nothing
  readDigits _ []
   = Nothing

testAtoiWf:: Atoi.AtoiT
          -> Property
testAtoiWf check
 = forAll nums
  (testAtoi check . BC.pack)
 where
  nums
   = do i <- elements [0..19]
        vectorOf i (elements ['0'..'9'])

testAtoiBorders :: Atoi.AtoiT -> Property
testAtoiBorders check
 = forAll border
 $ testAtoi check . BC.pack . show
 where
  border
   = do i :: Integer <- choose (0,65)
        j :: Integer <- choose (-1,1)
        k :: Integer <- elements [-1,1]
        return (k * (2 ^ i) + j)

testAtoiLeadingZero :: Atoi.AtoiT
          -> Property
testAtoiLeadingZero check
 = forAll nums
  (testAtoi check . BC.pack)
 where
  nums
   = do i <- elements [0..19]
        ds <- vectorOf i (elements ['0'..'9'])
        prefix <- elements [0..19]
        return (replicate prefix '0' <> ds)


prop_atoi_scalar
 = testAtoi Atoi.atoi_scalar

prop_atoi_scalar_wellformed
 = testAtoiWf Atoi.atoi_scalar

prop_atoi_scalar_borders
 = testAtoiBorders Atoi.atoi_scalar

prop_atoi_scalar_leading_zero
 = testAtoiLeadingZero Atoi.atoi_scalar




prop_atoi_vector128
 = testAtoi Atoi.atoi_vector128

prop_atoi_vector128_wellformed
 = testAtoiWf Atoi.atoi_vector128

prop_atoi_vector128_borders
 = testAtoiBorders Atoi.atoi_vector128

prop_atoi_vector128_leading_zero
 = testAtoiLeadingZero Atoi.atoi_vector128


return []
tests = $quickCheckAll
