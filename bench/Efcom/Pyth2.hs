module Efcom.Pyth2
  ( runPythTriples2
  ) where


import Control.Efcom
import Control.Efcom.Handle
import Control.Efcom.Escape


newtype Choose (efs :: [*]) (a :: *) = Choose [a]


data ListE
type instance Effect ListE = '[Handle Choose, Escape ()]


concatAcc :: forall a. [a] -> [[a]] -> [a]
concatAcc acc [] = acc
concatAcc acc (as : ass) = concatAcc (as ++ acc) ass


{-# INLINE concatR #-}
concatR :: forall a. [[a]] -> [a]
concatR = concatAcc []


chooseListHandler ::
  forall efs a. In (Escape () ListE) efs => Handler Choose efs [a]
chooseListHandler (Choose ms) h = chooseAll ms []
  where
    chooseAll [] acc = return $! concatR acc
    chooseAll (a : as) acc = do
      a' <- catchEscape @ListE @() (runComCont h a) (\ _ -> return [])
      chooseAll as $! a' : acc


{-# INLINE choose #-}
choose ::
  forall efs a. In (Handle Choose ListE) efs =>
  [a] -> Com efs a
choose as = backtrackHandle @ListE (Choose as)


{-# INLINE empty #-}
empty :: forall efs a. In (Escape () ListE) efs => Com efs a
empty = takeEscape @ListE ()


{-# INLINE runListE #-}
runListE :: forall efs a. Com (ListE : efs) a -> Com efs [a]
runListE m =
  runEscape
    (runHandle (runComHead m) return chooseListHandler)
    return
    (\ _ -> return [])


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


{-# NOINLINE runPythTriples2 #-}
runPythTriples2 :: Integer -> [(Integer, Integer, Integer)]
runPythTriples2 n = runComPure $ runListE (pythTriples [1..n])
