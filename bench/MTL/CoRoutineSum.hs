module MTL.CoRoutineSum
  ( runCoRoutineSum
  ) where

import Control.Monad.Trans.Cont
import Data.Functor.Identity


data LazyList x y a = LNil a | LCons y (x -> LazyList x y a)


{-# INLINE coyield #-}
coyield :: forall x y a. y -> Cont (LazyList x y a) x
coyield y = cont (LCons y)


{-# INLINE runCoRoutine #-}
runCoRoutine :: forall x y a. Cont (LazyList x y a) a -> LazyList x y a
runCoRoutine m = runCont m LNil


{-# NOINLINE coRoutineSum #-}
coRoutineSum ::
  Integer -> Integer -> Cont (LazyList () Integer ()) ()
coRoutineSum next final
  | next > final = return ()
  | otherwise = do
      count next
      coRoutineSum (next + 1) final
  where
    count :: Integer -> Cont (LazyList () Integer ()) ()
    count 0 = return ()
    count i = coyield 1 >> count (i-1)


{-# INLINE sumLazyList #-}
sumLazyList :: LazyList () Integer () -> Integer
sumLazyList (LNil _) = 0
sumLazyList (LCons x xs) = x + sumLazyList (xs ())


{-# NOINLINE runCoRoutineSum #-}
runCoRoutineSum :: Integer -> Integer
runCoRoutineSum n = sumLazyList (runCoRoutine (coRoutineSum 1 n))
