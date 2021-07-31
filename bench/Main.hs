module Main (main) where

import qualified Criterion.Main as C

import qualified Af.ReadWriteNoError as Af

import qualified MTL.ReadWriteNoError as MTL


readWriteNoError :: C.Benchmark
readWriteNoError =
  C.bgroup "ReadWriteNoError"
  [ C.bench "af" $ C.nf Af.runReadWriteNoError Af.readWriteNoError
  , C.bench "mtl" $ C.nf MTL.runReadWriteNoError MTL.readWriteNoError
  , C.bench "af" $ C.nf Af.runReadWriteNoError Af.readWriteNoError
  , C.bench "mtl" $ C.nf MTL.runReadWriteNoError MTL.readWriteNoError
  , C.bench "af" $ C.nf Af.runReadWriteNoError Af.readWriteNoError
  , C.bench "mtl" $ C.nf MTL.runReadWriteNoError MTL.readWriteNoError
  ]


main :: IO ()
main = C.defaultMain
  [ readWriteNoError
  ]
