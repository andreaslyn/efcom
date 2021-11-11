module Efcom.Pyth1
  ( runPythTriples1
  ) where


import Control.Efcom
import Control.Efcom.Handle
import Control.Efcom.State


data ListOps (efs :: [*]) (a :: *) = Empty | Choose ![a]


data ListE
type instance Effect ListE = '[Handle ListOps]


concatAcc :: forall a. [a] -> [[a]] -> [a]
concatAcc acc [] = acc
concatAcc acc (as : ass) = concatAcc (as ++ acc) ass


{-# INLINE concatR #-}
concatR :: forall a. [[a]] -> [a]
concatR = concatAcc []


listHandler :: forall efs a. Handler ListOps efs [a]
listHandler Empty _ = return []
listHandler (Choose choices) h = chooseAll choices []
  where
    chooseAll [] acc = return $! concatR acc
    chooseAll (a : as) acc = do
      a' <- runComCont h a
      chooseAll as $! a' : acc


{-# INLINE choose #-}
choose ::
  forall efs a. In (Handle ListOps ListE) efs =>
  [a] -> Com efs a
choose as = backtrackHandle @ListE (Choose as)


{-# INLINE empty #-}
empty :: forall efs a. In (Handle ListOps ListE) efs => Com efs a
empty = backtrackHandle @ListE Empty


{-# INLINE runListE #-}
runListE :: forall efs a. Com (ListE : efs) a -> Com efs [a]
runListE m = runHandle (runComHead m) return listHandler


{-# NOINLINE pythTriples #-}
pythTriples ::
  forall efs. In ListE efs =>
  [Integer] -> Com efs (Integer, Integer, Integer)
pythTriples ns = do
  x <- choose ns
  y <- choose ns
  z <- choose ns
  if x*x + y*y == z*z
  then return (x, y, z)
  else empty


{-# NOINLINE runPythTriples1 #-}
runPythTriples1 :: Integer -> [(Integer, Integer, Integer)]
runPythTriples1 n = runComPure $ runListE (pythTriples [1..n])
