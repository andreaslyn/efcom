module Efcom.CoRoutineSum
  ( runCoRoutineSum
  ) where

import Control.Efcom
import Control.Efcom.Handle


data CoYield :: * -> * -> [*] -> * -> * where
  CoYield :: forall x y efs. !y -> CoYield x y efs x


data CoRoutine x y
type instance Effect (CoRoutine x y) = '[Handle (CoYield x y)]


data LazyList x y efs a = LNil a | LCons y (x -> Com efs (LazyList x y efs a))


coyieldHandler :: forall x y efs a. Handler (CoYield x y) efs (LazyList x y efs a)
coyieldHandler (CoYield y) h = return (LCons y (runComCont h))


{-# INLINE coyield #-}
coyield :: forall x y efs. In (CoRoutine x y) efs => y -> Com efs x
coyield y = backtrackHandle @(CoRoutine x y) (CoYield y)


{-# INLINE runCoRoutine #-}
runCoRoutine :: forall x y efs a. Com (CoRoutine x y : efs) a -> Com efs (LazyList x y efs a)
runCoRoutine m = runHandle (runComHead m) LNil coyieldHandler


{-# NOINLINE coRoutineSum #-}
coRoutineSum ::
  forall efs. In (CoRoutine () Integer) efs =>
  Integer -> Integer -> Com efs ()
coRoutineSum next final
  | next > final = return ()
  | otherwise = do
      coyield @() next
      coyield @() next
      coyield @() next
      coRoutineSum (next + 1) final


{-# INLINE sumLazyList #-}
sumLazyList :: Com '[] (LazyList () Integer '[] ()) -> Integer
sumLazyList = sum 0
  where
    sum acc ef =
      case runComPure ef of
        LNil _ -> acc
        LCons x ef' -> (sum $! x + acc) (ef' ())


{-# NOINLINE runCoRoutineSum #-}
runCoRoutineSum :: Integer -> Integer
runCoRoutineSum n = sumLazyList (runCoRoutine (coRoutineSum 1 n))
