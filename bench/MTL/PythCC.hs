module MTL.PythCC
  ( runPythTriplesCC
  ) where

import Control.Monad.CC


{-# INLINE empty #-}
empty :: Prompt r [a] -> CC r b
empty pt = shift pt $ \ _ -> return []


--alt :: Prompt r [a] -> CC r b -> CC r b -> CC r b
--alt pt c1 c2 = shift pt $ \ k -> liftM2 (++) (k c1) (k c2)


choose :: Prompt r [a] -> [b] -> CC r b
choose pt as = shift pt (chooseAll as [])
  where
    chooseAll :: [b] -> [[a]] -> (CC r b -> CC r [a]) -> CC r [a]
    chooseAll [] acc = \ _ -> return $! foldr (flip (++)) [] acc
    chooseAll (a : as') acc = \ k -> do
      a' <- k (return a)
      (chooseAll as' $! a' : acc) k


{-# NOINLINE pythTriples #-}
pythTriples :: Prompt r [(Integer, Integer, Integer)] -> [Integer] -> CC r [(Integer, Integer, Integer)]
pythTriples pt ns = do
  x <- choose pt ns
  y <- choose pt ns
  z <- choose pt ns
  if x*x + y*y == z*z
  then return [(x, y, z)]
  else empty pt


{-# NOINLINE runPythTriplesCC #-}
runPythTriplesCC :: Integer -> [(Integer, Integer, Integer)]
runPythTriplesCC n = runCC (reset $ \ pt -> pythTriples pt [1..n])
