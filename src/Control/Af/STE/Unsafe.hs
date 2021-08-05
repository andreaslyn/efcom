module Control.Af.STE.Unsafe
  ( STE
  , runSTE
  , withSTE
  , AfEnv (..)
  , unsafeAfEnvError
  , unsafeAfEnvSuccess
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


{-# INLINE withSTE #-}
withSTE :: forall st a. Af '[STE st] a -> ST st a
withSTE af = ST (runAf# af)


{-# INLINE runSTE #-}
runSTE :: forall a. (forall st. Af '[STE st] a) -> a
runSTE af = runST (withSTE af)


data AfEnv s a =
    AfEnvError !(AfArray s) Any
  | AfEnvSuccess !(AfArray s) a


{-# INLINE unsafeAfEnvError #-}
unsafeAfEnvError :: forall s t a. AfArray s -> Any -> AfEnv t a
unsafeAfEnvError ar e = AfEnvError (unsafeCoerceAfArray ar) e


{-# INLINE unsafeAfEnvSuccess #-}
unsafeAfEnvSuccess :: forall s t a. AfArray s -> a -> AfEnv t a
unsafeAfEnvSuccess ar a = AfEnvSuccess (unsafeCoerceAfArray ar) a


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
    (# _, _, (# | | _ #) #) ->
      error "WHAT?"


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


{-# INLINE liftST #-}
liftST :: forall st es a. In (STE st) es => ST st a -> Af es a
liftST st = Af $ \ _ ar s0 ->
  let !(# s1, a #) = unST st (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# a | | #) #)
