module Af.CoRoutineSum
  ( runCoRoutineSum
  ) where

import Control.Af
import Control.Af.Handle


data CoYield :: * -> * -> [*] -> * -> * where
  CoYield :: forall x y efs. !y -> CoYield x y efs x


data CoRoutine x y
type instance Effect (CoRoutine x y) = '[Handle (CoYield x y)]


data LazyList x y efs a = LNil a | LCons y (x -> Af efs (LazyList x y efs a))


coyieldHandler :: forall x y efs a. Handler (CoYield x y) efs (LazyList x y efs a)
coyieldHandler (CoYield y) h = return (LCons y (runAfCont h))


{-# INLINE coyield #-}
coyield :: forall x y efs. In (CoRoutine x y) efs => y -> Af efs x
coyield y = backtrackHandle @(CoRoutine x y) (CoYield y)


{-# INLINE runCoRoutine #-}
runCoRoutine :: forall x y efs a. Af (CoRoutine x y : efs) a -> Af efs (LazyList x y efs a)
runCoRoutine m = runHandle (runAfHead m) LNil coyieldHandler


{-# NOINLINE coRoutineSum #-}
coRoutineSum ::
  forall efs. In (CoRoutine () Integer) efs =>
  Integer -> Integer -> Af efs ()
coRoutineSum next final
  | next > final = return ()
  | otherwise = do
      coyield @() next
      coyield @() next
      coyield @() next
      coRoutineSum (next + 1) final


{-# INLINE sumLazyList #-}
sumLazyList :: Af '[] (LazyList () Integer '[] ()) -> Integer
sumLazyList af =
  case runAfPure af of
    LNil _ -> 0
    LCons x af' -> x + sumLazyList (af' ())


{-# NOINLINE runCoRoutineSum #-}
runCoRoutineSum :: Integer -> Integer
runCoRoutineSum n = sumLazyList (runCoRoutine (coRoutineSum 1 n))
