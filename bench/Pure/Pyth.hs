module Pure.Pyth
  ( runPythTriples
  ) where


{-# NOINLINE pythTriples #-}
pythTriples :: [Integer] -> [(Integer, Integer, Integer)]
pythTriples ns = do
  x <- ns
  y <- ns
  z <- ns
  if x*x + y*y == z*z
  then [(x, y, z)]
  else []


{-# INLINE runPythTriples #-}
runPythTriples :: Integer -> [(Integer, Integer, Integer)]
runPythTriples n = pythTriples [1..n]
