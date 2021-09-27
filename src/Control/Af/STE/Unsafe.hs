module Control.Af.STE.Unsafe
  ( STE
  , runSTE
  , withSTE
  , AfEnv (..)
  , unsafeAfEnvError
  , unsafeAfEnvSuccess
  , unsafeAfEnvBacktrack
  , unsafeCoerceBacktrack
  , unST
  , AfToST
  , liftST
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Af
--import Control.Af.Internal.I16Pair
import Control.Af.Internal.In
import Control.Af.Internal.Util

import GHC.Exts (Any, State#)
import GHC.ST (ST (..), runST)

import qualified Unsafe.Coerce


{-# INLINE withSTE #-}
withSTE :: forall st a. Af '[STE st] a -> ST st a
withSTE af = ST (runAf# af)


{-# INLINE runSTE #-}
runSTE :: forall a. (forall st. Af '[STE st] a) -> a
runSTE af = runST (withSTE af)


{-# INLINE unsafeCoerceBacktrack #-}
unsafeCoerceBacktrack ::
  forall dfs dfs' efs efs' a.
  AfCont dfs Any efs a -> AfCont dfs' Any efs' a
unsafeCoerceBacktrack = Unsafe.Coerce.unsafeCoerce


data AfEnv s a =
    AfEnvError !(AfArray s) Any
  | AfEnvSuccess !(AfArray s) a
  | forall dfs efs. AfEnvBacktrack !(AfArray s) (AfCont dfs Any efs a)


instance Functor (AfEnv s) where
  fmap _ (AfEnvError ar e) = AfEnvError ar e
  fmap f (AfEnvSuccess ar a) = AfEnvSuccess ar (f a)
  fmap _ (AfEnvBacktrack ar k) = Unsafe.Coerce.unsafeCoerce (AfEnvBacktrack ar k)


{-# INLINE unsafeAfEnvError #-}
unsafeAfEnvError :: forall s t a. AfArray s -> Any -> AfEnv t a
unsafeAfEnvError ar e = AfEnvError (unsafeCoerceAfArray ar) e


{-# INLINE unsafeAfEnvSuccess #-}
unsafeAfEnvSuccess :: forall s t a. AfArray s -> a -> AfEnv t a
unsafeAfEnvSuccess ar a = AfEnvSuccess (unsafeCoerceAfArray ar) a


{-# INLINE unsafeAfEnvBacktrack #-}
unsafeAfEnvBacktrack ::
  forall s t dfs efs a.
  AfArray s -> AfCont dfs Any efs a -> AfEnv t a
unsafeAfEnvBacktrack ar k =
  AfEnvBacktrack (unsafeCoerceAfArray ar) (unsafeCoerceBacktrack k)


{-# INLINE unST #-}
unST :: forall st a. ST st a -> State# st -> (# State# st, a #)
unST (ST f) = f


type AfToST st es = forall a. Af es a -> ST st (AfEnv st a)


{-# INLINE liftST #-}
liftST :: forall st es a. In (STE st) es => ST st a -> Af es a
liftST st = Af $ \ _ ar s0 ->
  let !(# s1, a #) = unST st (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# a | #) #)


-- TODO missing transST or similar, see IOE/Unsafe.hs.
