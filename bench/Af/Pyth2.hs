module Af.Pyth2
  ( runPythTriples2
  ) where


import Control.Af
import Control.Af.Handle
import Control.Af.Escape


newtype Choose (efs :: [*]) (a :: *) = Choose [a]


data ListE
type instance Effect ListE = '[Handle Choose, Escape ()]


chooseListHandler ::
  forall efs a. In (Escape () ListE) efs => Handler Choose efs [a]
chooseListHandler (Choose ms) h = chooseAll ms []
  where
    chooseAll [] acc = return $! foldr (flip (++)) [] acc
    chooseAll (a : as) acc = do
      a' <- catchEscape @ListE @() (runAfCont h a) (\ _ -> return [])
      chooseAll as $! a' : acc


{-# INLINE choose #-}
choose ::
  forall efs a. In (Handle Choose ListE) efs =>
  [a] -> Af efs a
choose as = backtrackHandle @ListE (Choose as)


{-# INLINE empty #-}
empty :: forall efs a. In (Escape () ListE) efs => Af efs a
empty = takeEscape @ListE ()


{-# INLINE runListE #-}
runListE :: forall efs a. Af (ListE : efs) a -> Af efs [a]
runListE m =
  runEscape
    (runHandle (runAfHead m) return chooseListHandler)
    return
    (\ _ -> return [])


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


{-# NOINLINE runPythTriples2 #-}
runPythTriples2 :: Integer -> [(Integer, Integer, Integer)]
runPythTriples2 n = runAfPure $ runListE (pythTriples [1..n])
