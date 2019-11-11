{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Memcmp where

import qualified  Anemone.Foreign.Memcmp.Export as Export

import            P
import            Test.QuickCheck
import            Test.QuickCheck.Instances()

import qualified  Data.ByteString as B

testCmp   :: (B.ByteString -> B.ByteString -> Ordering)
          -> B.ByteString -> B.ByteString -> B.ByteString -> Property
testCmp check prefix a b
 = let a' = pad a
       b' = pad b
       a'' = prefix <> a'
       b'' = prefix <> b'
   in  counterexample (show (a',b'))
       ( compare a' b' === check a' b')
       -- Testing with an equal same prefix makes it more likely to hit weird cases
  .&&. counterexample (show (a'',b''))
       ( compare a'' b'' === check a'' b'')
 where
  len = min (B.length a) (B.length b)
  pad x = B.take len x

prop_memcmp
 = testCmp Export.memcmp

prop_memcmp8
 = testCmp Export.memcmp8
prop_memcmp64
 = testCmp Export.memcmp64
prop_memcmp128_unsafe
 = testCmp Export.memcmp128_unsafe
prop_memcmp_partial_load64
 = testCmp Export.memcmp_partial_load64


return []
tests = $quickCheckAll
