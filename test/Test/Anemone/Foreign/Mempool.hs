{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Mempool where

import Anemone.Foreign.Data
import qualified Anemone.Foreign.Mempool as Mempool
import qualified Foreign.Marshal.Array as Marshal

import            P
import            Test.QuickCheck
import            Test.QuickCheck.Monadic
import            Test.QuickCheck.Instances()

import qualified  Data.List as List

foreign import ccall unsafe
    test_mempool_free
    :: CInt -> CInt -> CSize -> CBool

prop_mempool_free :: Property
prop_mempool_free
 = forAll iterations $ \a ->
   forAll iterations $ \b ->
   forAll megabytes $ \c ->
   test_mempool_free a b c /= 0

foreign import ccall unsafe
    test_mempool_nonoverlap
    :: CInt -> CSize -> CBool

foreign import ccall unsafe
    test_mempool_size
    :: CInt -> CSize -> CBool


prop_mempool_nonoverlap :: Property
prop_mempool_nonoverlap
 = forAll iterations $ \a ->
   forAll megabytes $ \b ->
   test_mempool_nonoverlap a b /= 0


prop_mempool_track_size :: Property
prop_mempool_track_size
 = forAll iterations $ \a ->
   forAll megabytes $ \b ->
   test_mempool_size a b /= 0


-- Really simple sanity test of the FFI
-- Create a pool, allocate a whole bunch of pointers and make sure the pointers are distinct
prop_mempool_sanity :: Property
prop_mempool_sanity
 = forAll (listOf megabytes) $ \sizes ->
  monadicIO . run $ do
    pool <- Mempool.create
    vals <- mapM (Mempool.allocBytes pool) sizes
    Mempool.free pool
    -- If all pointers are distinct, length of uniques should be same as original length
    let uniqs = List.nub vals
    return (counterexample (show vals) $ length vals === length uniqs)

-- Create a pool, allocate a whole bunch of pointers and make sure the pointers are all aligned
prop_mempool_all_aligned :: Property
prop_mempool_all_aligned
 = forAll (listOf iterationSizes) $ \sizes ->
   monadicIO . run $ do
    pool <- Mempool.create
    vals <- mapM (Mempool.allocBytes pool) sizes
    Mempool.free pool
    -- If all pointers are aligned.
    nonAligned <- List.map snd . List.filter (not . fst)
                    <$> mapM (\v -> (,) <$> (fromCBool <$> Mempool.isPointerAligned v) <*> pure v) vals
    return (counterexample (show nonAligned) $ List.null nonAligned === True)


prop_mempool_calloc :: Property
prop_mempool_calloc
 -- generate a CSize big enough, but not too big.
 -- 2 megabytes is a bit too big to convert to a list.
 = forAll (choose (1, 1000)) $ \num_items ->
   monadicIO . run $ do
    pool <- Mempool.create
    arr <- Mempool.calloc pool (fromIntegral num_items)
    vals <- Marshal.peekArray num_items arr
    Mempool.free pool
    return (vals === List.replicate num_items (CInt 0))

-- Really simple sanity test of the FFI: make sure totalAllocSize returns something,
-- but don't really care what since prop_mempool_track_size checks the value
prop_mempool_sanity_total_alloc :: Property
prop_mempool_sanity_total_alloc
 = forAll megabytes $ \sizeToAlloc ->
   monadicIO . run $ do
    pool <- Mempool.create
    _ <- Mempool.allocBytes pool sizeToAlloc
    poolSize <- Mempool.totalAllocSize pool
    Mempool.free pool
    return (poolSize >= fromIntegral sizeToAlloc)


iterations :: Gen CInt
iterations = choose (1, 100)

iterationSizes :: Gen CSize
iterationSizes = choose (1, 100)

-- Choose some number of bytes from 1 to 2mb
-- (2mb is larger than block size)
megabytes :: Gen CSize
megabytes = choose (0, 2 * 1024 * 1024)

return []
tests = $quickCheckAll
