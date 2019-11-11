{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Anemone.Foreign.VInt where

import           Anemone.Foreign.VInt

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector.Storable as Storable

import           Hedgehog
import qualified Hedgehog.Gen as Gen
-- import           Hedgehog.Gen.QuickCheck (arbitrary)
import qualified Hedgehog.Range as Range

import           P

import qualified Prelude as Savage

import           System.IO (IO)

import           Test.QuickCheck.Instances ()


prop_roundtrip_vint_builder :: Property
prop_roundtrip_vint_builder =
  property $ do
    a <- forAll Gen.enumBounded
    tripping a (Lazy.toStrict . Builder.toLazyByteString . bVInt) (fmap noLeftovers . decodeVInt)

prop_roundtrip_vint :: Property
prop_roundtrip_vint =
  property $ do
    a <- forAll Gen.enumBounded
    tripping a encodeVInt (fmap noLeftovers . decodeVInt)

prop_roundtrip_vint_array :: Property
prop_roundtrip_vint_array =
  property $ do
    as <- forAll $ Gen.list (Range.linear 1 10000) Gen.enumBounded
    let
      ss = Storable.fromList as
    tripping ss encodeVIntArray (fmap noLeftovers . decodeVIntArray (Storable.length ss))

noLeftovers :: (a, ByteString) -> a
noLeftovers (xs, bs) =
  if B.null bs then
    xs
  else
    Savage.error $ "prop_roundtrip_vint: unexpected leftover bytes " <> show bs

-- prop_unpack_safe :: Property
-- prop_unpack_safe =
--   property $ do
--     n  <- forAll $ Gen.int (Range.linear (-1) 10000000)
--     bs <- forAll arbitrary
--     assert $
--       case decodeVIntArray n bs of
--         Nothing ->
--           True
--         Just _ ->
--           True

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)