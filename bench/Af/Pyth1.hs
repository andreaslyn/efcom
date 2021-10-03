module Af.Pyth1
  ( runPythTriples1
  ) where


import Control.Af
import Control.Af.Handle
import Control.Af.State


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
      a' <- runAfCont h a
      chooseAll as $! a' : acc


{-# INLINE choose #-}
choose ::
  forall efs a. In (Handle ListOps ListE) efs =>
  [a] -> Af efs a
choose as = backtrackHandle @ListE (Choose as)


{-# INLINE empty #-}
empty :: forall efs a. In (Handle ListOps ListE) efs => Af efs a
empty = backtrackHandle @ListE Empty


{-# INLINE runListE #-}
runListE :: forall efs a. Af (ListE : efs) a -> Af efs [a]
runListE m = runHandle (runAfHead m) return listHandler


{-# NOINLINE pythTriples #-}
pythTriples ::
  forall efs. In ListE efs =>
  [Integer] -> Af efs (Integer, Integer, Integer)
pythTriples ns = do
  x <- choose ns
  y <- choose ns
  z <- choose ns
  if x*x + y*y == z*z
  then return (x, y, z)
  else empty


{-# NOINLINE runPythTriples1 #-}
runPythTriples1 :: Integer -> [(Integer, Integer, Integer)]
runPythTriples1 n = runAfPure $ runListE (pythTriples [1..n])
