module Main (main) where

import qualified Criterion.Main as C

import qualified Pure.Countdown as Pure
import qualified Pure.Pyth as Pure

import qualified Af.ReadWriteNoError as Af
import qualified Af.Countdown as Af
import qualified Af.Pyth1 as Af
import qualified Af.Pyth2 as Af
import qualified Af.CoRoutineSum as Af

import qualified MTL.ReadWriteNoError as MTL
import qualified MTL.Countdown as MTL
import qualified MTL.Pyth1 as MTL
import qualified MTL.Pyth2 as MTL
import qualified MTL.CoRoutineSum as MTL


import Debug.Trace (trace)


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
  [ C.bench "pure" $ C.nf Pure.runCountdownCountupExc n
  , C.bench "af" $ C.nf Af.runCountdownCountupExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownCountupExc n
  , C.bench "pure" $ C.nf Pure.runCountdownCountupExc n
  , C.bench "af" $ C.nf Af.runCountdownCountupExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownCountupExc n
  , C.bench "pure" $ C.nf Pure.runCountdownCountupExc n
  , C.bench "af" $ C.nf Af.runCountdownCountupExc n
  , C.bench "mtl" $ C.nf MTL.runCountdownCountupExc n
  ]


pythTriples :: C.Benchmark
pythTriples =
  let n = 50 in
  C.bgroup "PythTriples"
  [ C.bench "pure" $ C.nf Pure.runPythTriples n
  , C.bench "af-1" $ C.nf Af.runPythTriples1 n
  , C.bench "af-2" $ C.nf Af.runPythTriples2 n
  , C.bench "mtl-1" $ C.nf MTL.runPythTriples1 n
  , C.bench "mtl-2" $ C.nf MTL.runPythTriples2 n
  , C.bench "pure" $ C.nf Pure.runPythTriples n
  , C.bench "af-1" $ C.nf Af.runPythTriples1 n
  , C.bench "af-2" $ C.nf Af.runPythTriples2 n
  , C.bench "mtl01" $ C.nf MTL.runPythTriples1 n
  , C.bench "mtl-2" $ C.nf MTL.runPythTriples2 n
  , C.bench "pure" $ C.nf Pure.runPythTriples n
  , C.bench "af-1" $ C.nf Af.runPythTriples1 n
  , C.bench "af-2" $ C.nf Af.runPythTriples2 n
  , C.bench "mtl-1" $ C.nf MTL.runPythTriples1 n
  , C.bench "mtl-2" $ C.nf MTL.runPythTriples2 n
  ]


coRoutineSum :: C.Benchmark
coRoutineSum =
  let n = 500 in
  C.bgroup "CoRoutineSum"
  [ C.bench "af" $ C.nf Af.runCoRoutineSum n
  , C.bench "mtl" $ C.nf MTL.runCoRoutineSum n
  , C.bench "af" $ C.nf Af.runCoRoutineSum n
  , C.bench "mtl" $ C.nf MTL.runCoRoutineSum n
  , C.bench "af" $ C.nf Af.runCoRoutineSum n
  , C.bench "mtl" $ C.nf MTL.runCoRoutineSum n
  ]


main :: IO ()
main = C.defaultMain
  --[ countdownPut
  --[ countdownExc
  --[ readWriteNoError
  --[ pythTriples
  --[ coRoutineSum
  [ countdownCountupExc
  , countdownPut
  , countdownExc
  , readWriteNoError
  , pythTriples
  , coRoutineSum
  ]
