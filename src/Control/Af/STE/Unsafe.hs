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
  , unsafeAfToST
  , controlST
  , liftST
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Af
import Control.Af.Internal.I16Pair
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
  forall dfs efs dfs' efs' a.
  (Af dfs Any -> Af efs a) -> Af dfs' Any -> Af efs' a
unsafeCoerceBacktrack = Unsafe.Coerce.unsafeCoerce


data AfEnv s a =
    AfEnvError !(AfArray s) Any
  | AfEnvSuccess !(AfArray s) a
  | forall dfs efs. AfEnvBacktrack !(AfArray s) Any (Af dfs Any -> Af efs a)


instance Functor (AfEnv s) where
  fmap _ (AfEnvError ar e) = AfEnvError ar e
  fmap f (AfEnvSuccess ar a) = AfEnvSuccess ar (f a)
  fmap _ (AfEnvBacktrack ar op k) = Unsafe.Coerce.unsafeCoerce (AfEnvBacktrack ar op k)


{-# INLINE unsafeAfEnvError #-}
unsafeAfEnvError :: forall s t a. AfArray s -> Any -> AfEnv t a
unsafeAfEnvError ar e = AfEnvError (unsafeCoerceAfArray ar) e


{-# INLINE unsafeAfEnvSuccess #-}
unsafeAfEnvSuccess :: forall s t a. AfArray s -> a -> AfEnv t a
unsafeAfEnvSuccess ar a = AfEnvSuccess (unsafeCoerceAfArray ar) a


{-# INLINE unsafeAfEnvBacktrack #-}
unsafeAfEnvBacktrack ::
  forall s t dfs efs a.
  AfArray s -> Any -> (Af dfs Any -> Af efs a) -> AfEnv t a
unsafeAfEnvBacktrack ar op k =
  AfEnvBacktrack (unsafeCoerceAfArray ar) op (unsafeCoerceBacktrack k)


{-# INLINE unST #-}
unST :: forall st a. ST st a -> State# st -> (# State# st, a #)
unST (ST f) = f


type AfToST st es = forall a. Af es a -> ST st (AfEnv st a)


{-# INLINE unsafeAfToST #-}
unsafeAfToST :: forall s st es. I16Pair -> AfArray s -> AfToST st es
unsafeAfToST sz ar = \ af -> ST $ \ s ->
  case unAf af sz ar (unsafeCoerceState s) of
    (# ar', s', (# a | | #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvSuccess ar' a #)
    (# ar', s', (# | e | #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvError ar' e #)
    (# ar', s', (# | | (# op, k #) #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvBacktrack ar' op k #)


{-# INLINE controlST #-}
controlST ::
  forall st es a. In (STE st) es =>
  (AfToST st es -> ST st (AfEnv st a)) -> Af es a
controlST f = Af $ \ sz ar0 s0 ->
  case unST (f (unsafeAfToST sz ar0)) (unsafeCoerceState s0) of
    (# s1, AfEnvError ar1 e #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# | e | #) #)
    (# s1, AfEnvSuccess ar1 a #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# a | | #) #)
    (# s1, AfEnvBacktrack ar1 op k #) ->
      (# unsafeCoerceAfArray ar1
       , unsafeCoerceState s1
       , (# | | (# op, unsafeCoerceBacktrack k #) #) #)


{-# INLINE liftST #-}
liftST :: forall st es a. In (STE st) es => ST st a -> Af es a
liftST st = Af $ \ _ ar s0 ->
  let !(# s1, a #) = unST st (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# a | | #) #)
