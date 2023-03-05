{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn #-}

import qualified Anemone.Foreign.FFI as FoFFI
import qualified Anemone.Foreign.Hash as FoHash
import qualified Anemone.Foreign.Time as FoTime
import qualified Bench.Foreign.Memcmp as FoMemcmp
import qualified Bench.Foreign.Atoi as FoAtoi
import qualified Bench.Foreign.Strtod as FoStrtod

import           Criterion.Main
import           Criterion.Types (Config(..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import           Data.Hashable (hash)
import           Data.Thyme (Day, YearMonthDay, defaultTimeLocale)
import qualified Data.Thyme.Format as Thyme

import           System.IO (IO)

import           P


main :: IO ()
main
 = defaultMainWith benchConfig
 [ bench_ffi
 , bench_memcmp
 , bench_atoi
 , bench_strtod
 , bench_hash
 , bench_time
 ]

benchConfig :: Config
benchConfig =
  defaultConfig

bench_ffi :: Benchmark
bench_ffi
 = bgroup "Foreign function interface"
 [ bench "None (return unit)"       $ nfIO $ loop $ (return ())
 , bench "Pure (id 0)"              $ nfIO $ loop $ (return $ id (0 :: Int))
 , bench "Safe (ffi_id_safe)"       $ nfIO $ loop $ (return $ FoFFI.ffi_id_safe   0)
 , bench "Unsafe (ffi_id_unsafe)"   $ nfIO $ loop $ (return $ FoFFI.ffi_id_unsafe 0)
 ]

 where
  loop f
   = let go 0 = return ()
         go n = f >>= \a -> seq a (go (n - 1))
     in  go (100000 :: Int)


bench_memcmp :: Benchmark
bench_memcmp
 = bgroup "Memory comparisons"
 [ bgroup "Simple bench"
     [ go_all "memcmp8"                 FoMemcmp.memcmp8_simple_bench
     , go_all "memcmp64"                FoMemcmp.memcmp64_simple_bench
     , go_all "memcmp128"               FoMemcmp.memcmp128_unsafe_simple_bench
     , go_all "memcmp_partial_load64"   FoMemcmp.memcmp_partial_load64_simple_bench
     , go_all "memcmp std"              FoMemcmp.memcmp_std_simple_bench
     ]
 , bgroup "Register pressure"
     [ go_all "memcmp8"                 FoMemcmp.memcmp8_regs_bench
     , go_all "memcmp64"                FoMemcmp.memcmp64_regs_bench
     , go_all "memcmp128"               FoMemcmp.memcmp128_unsafe_regs_bench
     , go_all "memcmp_partial_load64"   FoMemcmp.memcmp_partial_load64_regs_bench
     , go_all "memcmp std"              FoMemcmp.memcmp_std_regs_bench
     ]
  ]

 where
  go_all nm f
   = bgroup nm
   -- Don't use round numbers because we want to test the leftovers in memcmp64
   [ bench "5"  $ check2 f 5
   , bench "10" $ check2 f 10
   , bench "40" $ check2 f 40
   , bench "160" $ check2 f 160
   ]

  check2 f n
   = nf f n


bench_atoi :: Benchmark
bench_atoi
 = bgroup "String to Int64"
 [ go_all "v-1"                 FoAtoi.string_to_i64_bench
 , go_all "v-128"               FoAtoi.string_to_i64_v128_bench
 , go_all "stdlib"              FoAtoi.string_to_i64_std_bench
 ]

 where
  go_all nm f
   = bgroup nm
   $ fmap (\str -> bench (show str) $ nf f (pad str))
   [ "0", "1", "-123", "123456", "-12345678", "1234567890", "-123456789012345", "1234567890123456789", "12345678901234567890"
   ]
  pad str = str <> " (padding added) "


bench_strtod :: Benchmark
bench_strtod
 = bgroup "String to Double"
 [ go_all "v-128"               FoStrtod.strtod_bench
 , go_all "stdlib"              FoStrtod.strtod_std_bench
 , go_all "Text.Read"           FoStrtod.strtod_hs_read_double_bench
 ]

 where
  go_all nm f
   = bgroup nm
   $ fmap (\str -> bench (show str) $ nf f (pad str))
   [ "0", "1", "-123", "123456", "-12345678", "1234567890", "-123456789012345"
   , "0.0" , "0.12345678" , "0.1234567890123456"
   , "123.123", "1234567.1234567", "1234567890.1234567890"
   ]
  pad str = str <> " (padding added) "

bench_hash :: Benchmark
bench_hash
 = bgroup "Hashing"
 [ go_all "fasthash32"            (fromIntegral . FoHash.fasthash32)
 , go_all "fasthash64"            (fromIntegral . FoHash.fasthash64)
 , go_all "hashable+fnv1"         hash
 ]

 where
  go_all nm (f :: ByteString -> Int)
   = bgroup nm
   $ fmap (\str -> bench (show (B.length str) <> "B") $ nf f str)
   [ "12345678"
   , "1234567890123456"
   , "123456789012345678901234"
   , "12345678901234567890123456789012"
   , "1234567890123456789012345678901234567890"
   , "123456789012345678901234567890123456789012345678"
   , "12345678901234567890123456789012345678901234567890123456"
   , "1234567890123456789012345678901234567890123456789012345678901234"
   ]

bench_time :: Benchmark
bench_time
 = bgroup "Date Parsing"
 [ go_all "Anemone.parseDay" (fmap fst . FoTime.parseDay)
 , go_all "Anemone.parseYearMonthDay" (fmap fst . FoTime.parseYearMonthDay)
 , go_all "Thyme.parseTime :: Day" thymeDay
 , go_all "Thyme.parseTime :: YearMonthDay" thymeYearMonthDay
 ]

 where
  go_all nm (f :: ByteString -> Either x a)
   = bgroup nm
   $ fmap (\str -> bench (Char8.unpack str) $ nf f str)
   [ "2016-12-01"
   , "2016-02-28"
   , "2016-02-29"
   , "2016-03-31"
   , "2016-00-00"
   ]

  thymeDay :: ByteString -> Either () Day
  thymeDay
   = maybe (Left ()) Right
   . Thyme.parseTime defaultTimeLocale "%Y-%m-%d" . Char8.unpack

  thymeYearMonthDay :: ByteString -> Either () YearMonthDay
  thymeYearMonthDay
   = maybe (Left ()) Right
   . Thyme.parseTime defaultTimeLocale "%Y-%m-%d" . Char8.unpack
