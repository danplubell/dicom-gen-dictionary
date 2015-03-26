module Main (main) where

import qualified Dicom.Gen.DictionaryBench
-- HASKELETON: import qualified New.ModuleBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
    [ bgroup "Dicom.Gen.Dictionary" Dicom.Gen.DictionaryBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
