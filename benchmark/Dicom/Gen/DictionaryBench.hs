module Dicom.Gen.DictionaryBench (benchmarks) where

import Dicom.Gen.Dictionary

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "main" (nfIO main)
    ]
