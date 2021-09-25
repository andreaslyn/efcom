module Main (main) where

import qualified Criterion.Main as C

import qualified Pure.Countdown as Pure

import qualified Af.ReadWriteNoError as Af
import qualified Af.Countdown as Af

import qualified MTL.ReadWriteNoError as MTL
import qualified MTL.Countdown as MTL


readWriteNoError :: C.Benchmark
readWriteNoError =
  let n = 50000 in
  C.bgroup "ReadWriteNoError"
  [ C.bench "af" $ C.nf Af.runReadWriteNoError n
  , C.bench "mtl" $ C.nf MTL.runReadWriteNoError n
  , C.bench "af" $ C.nf Af.runReadWriteNoError n
  , C.bench "mtl" $ C.nf MTL.runReadWriteNoError n
  , C.bench "af" $ C.nf Af.runReadWriteNoError n
  , C.bench "mtl" $ C.nf MTL.runReadWriteNoError n
  ]


countdownPut :: C.Benchmark
countdownPut =
  let n = 500000 in
  C.bgroup "CountdownPut"
  [ C.bench "pure" $ C.nf Pure.runCountdownPut n
  , C.bench "af" $ C.nf Af.runCountdownPut n
  , C.bench "mtl" $ C.nf MTL.runCountdownPut n
  , C.bench "pure" $ C.nf Pure.runCountdownPut n
  , C.bench "af" $ C.nf Af.runCountdownPut n
  , C.bench "mtl" $ C.nf MTL.runCountdownPut n
  , C.bench "pure" $ C.nf Pure.runCountdownPut n
  , C.bench "af" $ C.nf Af.runCountdownPut n
  , C.bench "mtl" $ C.nf MTL.runCountdownPut n
  ]


countdownExc :: C.Benchmark
countdownExc =
  let n = 500000 in
  C.bgroup "CountdownExc"
  [ C.bench "pure" $ C.nf Pure.runCountdownExc n
  , C.bench "af" $ C.nf Af.runCountdownExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownExc n
  , C.bench "pure" $ C.nf Pure.runCountdownExc n
  , C.bench "af" $ C.nf Af.runCountdownExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownExc n
  , C.bench "pure" $ C.nf Pure.runCountdownExc n
  , C.bench "af" $ C.nf Af.runCountdownExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownExc n
  ]


countdownCountupExc :: C.Benchmark
countdownCountupExc =
  let n = 500000 in
  C.bgroup "CountdownCountupExc"
  [ C.bench "pure" $ C.nf Af.runCountdownCountupExc n
  , C.bench "af" $ C.nf Af.runCountdownCountupExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownCountupExc n
  , C.bench "pure" $ C.nf Af.runCountdownCountupExc n
  , C.bench "af" $ C.nf Af.runCountdownCountupExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownCountupExc n
  , C.bench "pure" $ C.nf Af.runCountdownCountupExc n
  , C.bench "af" $ C.nf Af.runCountdownCountupExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownCountupExc n
  ]


main :: IO ()
main = C.defaultMain
  --[ countdownPut
  --[ countdownExc
  --[ readWriteNoError
  [ countdownCountupExc
  , countdownPut
  , countdownExc
  , readWriteNoError
  ]
