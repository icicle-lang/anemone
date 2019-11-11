{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Anemone.Foreign.Time where

import           Anemone.Foreign.Time

import qualified Data.ByteString.Char8 as Char8
import           Data.Thyme.Calendar (Day(..), YearMonthDay(..), gregorianValid)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Gen.QuickCheck (arbitrary)

import           P

import           Text.Printf (printf)
import           System.IO (IO)


mkDay :: YearMonthDay -> Either TimeError Day
mkDay ymd =
  case gregorianValid ymd of
    Nothing ->
      Left $ TimeInvalidDate ymd
    Just day ->
      Right day

prop_parseYearMonthDay :: Property
prop_parseYearMonthDay = property $ do
  y  <- forAll $ Gen.int (Range.linear 0 9999)
  m  <- forAll $ Gen.int (Range.linear 1 12)
  d  <- forAll $ Gen.int (Range.linear 1 31)
  xs <- forAll $ Gen.list (Range.linear 0 100) arbitrary
  let
    ymd =
      YearMonthDay y m d

    eymd =
      fmap (const (ymd, Char8.pack xs)) $ mkDay ymd

    str =
      Char8.pack $ printf "%04d-%02d-%02d%s" y m d xs

  parseYearMonthDay str === eymd

prop_parseDay :: Property
prop_parseDay = property $ do
  y  <- forAll $ Gen.int (Range.linear 0 9999)
  m  <- forAll $ Gen.int (Range.linear 1 12)
  d  <- forAll $ Gen.int (Range.linear 1 31)
  xs <- forAll $ Gen.list (Range.linear 0 100) arbitrary
  let
    ymd =
      YearMonthDay y m d

    eday =
      fmap (, Char8.pack xs) $ mkDay ymd

    str =
      Char8.pack $ printf "%04d-%02d-%02d%s" y m d xs

  parseDay str === eday

prop_parseRenderError :: Property
prop_parseRenderError = property $ do
  y  <- forAll $ Gen.int (Range.linear 0 9999)
  m  <- forAll $ Gen.int (Range.linear 1 12)
  d  <- forAll $ Gen.int (Range.linear 1 31)
  xs <- forAll $ Gen.list (Range.linear 0 100) arbitrary
  let
    ymd =
      YearMonthDay y m d

    eymd =
      fmap (const (ymd, Char8.pack xs)) $ mkDay ymd

    str =
      Char8.pack $ printf "%04d-%02d-%02d%s" y m d xs

  first renderTimeError (parseYearMonthDay str) === first renderTimeError eymd

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)