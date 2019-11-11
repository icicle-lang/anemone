{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Anemone.Foreign.Pack where

import           Anemone.Foreign.Pack

import qualified Data.List as List
import qualified Data.Vector.Storable as Storable
import           Data.Word (Word64)

import           P

import qualified Prelude

import           Test.QuickCheck (Property, NonEmptyList(..), (===), counterexample)
import           Test.QuickCheck (forAllProperties, quickCheckWithResult)
import           Test.QuickCheck (stdArgs, maxSuccess)
import           Test.QuickCheck.Instances ()


tripping :: (Applicative f, Show (f a), Eq (f a)) => (a -> b) -> (b -> f a) -> a -> Property
tripping to fro a =
  let tripped = (fro . to) a
      purea   = pure a
  in counterexample (show tripped <> " /= " <> show purea)
     (tripped === purea)

prop_pack_unpack_tripping (n :: Int) (NonEmpty (xs :: [Word64])) =
 tripping (impossible . pack64) unpack64 ys
 where
  ys
   = Storable.fromList
   $ List.take (n * 64)
   $ List.cycle xs

  impossible
   = fromMaybe
     (Prelude.error $ "prop_pack_unpack_tripping: could not encode " <> show n <> " integers")

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10000})
