module MTL.PythContT
  ( runPythTriplesContT
  ) where

import Control.Monad.Trans.Cont


choose :: [Integer] -> Cont [r] Integer
choose as = cont (chooseAll as [])
  where
    chooseAll [] acc = \ _ -> foldr (flip (++)) [] acc
    chooseAll (a : as') acc = \ k -> do
      let a' = k a
      (chooseAll as' $! a' : acc) k


{-# NOINLINE pythTriples #-}
pythTriples :: [Integer] -> Cont [r] (Integer, Integer, Integer)
pythTriples ns = do
  x <- choose ns
  y <- choose ns
  z <- choose ns
  if x*x + y*y == z*z
  then return (x, y, z)
  else cont (\ _ -> [])


{-# NOINLINE runPythTriplesContT #-}
runPythTriplesContT :: Integer -> [(Integer, Integer, Integer)]
runPythTriplesContT n = runCont (pythTriples [1..n]) return
