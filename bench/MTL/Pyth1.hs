{-# OPTIONS_GHC -Wno-orphans #-}

module MTL.Pyth1
  ( runPythTriples1
  ) where

import Control.Monad.Trans.Cont
import Control.Applicative
import Control.Monad


instance Monad m => Alternative (ContT [r] m) where
  empty = ContT $ \ _ -> return []
  ContT m1 <|> ContT m2 = ContT $ \k -> liftM2 (++) (m1 k) (m2 k)


choose :: [Integer] -> Cont [r] Integer
choose [] = empty
choose (x : xs) = return x <|> choose xs


{-# NOINLINE pythTriples #-}
pythTriples :: [Integer] -> Cont [r] (Integer, Integer, Integer)
pythTriples ns = do
  x <- choose ns
  y <- choose ns
  z <- choose ns
  if x*x + y*y == z*z
  then return (x, y, z)
  else empty


{-# NOINLINE runPythTriples1 #-}
runPythTriples1 :: Integer -> [(Integer, Integer, Integer)]
runPythTriples1 n = runCont (pythTriples [1..n]) return
