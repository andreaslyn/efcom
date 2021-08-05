module Control.Af.Internal.Af
  ( Af (..)
  , runAf#
  , runAfPure
  , runAfHead
  ) where

import Control.Af.Internal.I16Pair
import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray

import GHC.Exts (Any, State#)
import qualified GHC.Exts as GHC

import Unsafe.Coerce (unsafeCoerce)


newtype Af (efs :: [*]) (a :: *) = Af
  { unAf ::
      forall dfs s.
      I16Pair -> AfArray s -> State# s ->
      (# AfArray s, State# s, (# a | Any | Af dfs Any -> Af efs a #) #)
  }


{-# NOINLINE fmapAf #-}
fmapAf :: forall efs a b. (a -> b) -> Af efs a -> Af efs b
fmapAf f af = Af $ \ sz ar s ->
  case unAf af sz ar s of
    (# ar', s', (# a | | #) #) ->
      (# ar', s', (# f a | | #) #)
    (# ar', s', (# | e | #) #) ->
      (# ar', s', (# | e | #) #)
    (# ar', s', (# | | k #) #) ->
      (# ar', s', (# | | \ x -> fmapAf f (k x) #) #)


instance Functor (Af efs) where
  {-# INLINE fmap #-}
  fmap f af = Af $ \ sz ar s ->
    case unAf af sz ar s of
      (# ar', s', (# a | | #) #) ->
        (# ar', s', (# f a | | #) #)
      (# ar', s', (# | e | #) #) ->
        (# ar', s', (# | e | #) #)
      (# ar', s', (# | | k #) #) ->
        (# ar', s', (# | | \ x -> fmapAf f (k x) #) #)


{-# NOINLINE apAf #-}
apAf :: forall efs a b. Af efs (a -> b) -> Af efs a -> Af efs b
apAf ff af = Af $ \ sz ar s ->
  case unAf ff sz ar s of
    (# ar1, s1, (# f | | #) #) ->
      case unAf af sz ar1 s1 of
        (# ar2, s2, (# a | | #) #) -> (# ar2, s2, (# f a | | #) #)
        (# ar2, s2, (# | e | #) #) -> (# ar2, s2, (# | e | #) #)
        (# ar2, s2, (# | | k #) #) -> (# ar2, s2, (# | | \ x -> fmapAf f (k x) #) #)
    (# ar1, s1, (# | e | #) #) ->
      (# ar1, s1, (# | e | #) #)
    (# ar1, s1, (# | | k #) #) ->
      (# ar1, s1, (# | | \ x -> apAf (k x) af #) #)


instance Applicative (Af efs) where
  {-# INLINE pure #-}
  pure a = Af $ \ _ ar s -> (# ar, s, (# a | | #) #)

  {-# INLINE (<*>) #-}
  ff <*> af = Af $ \ sz ar s ->
    case unAf ff sz ar s of
      (# ar1, s1, (# f | | #) #) ->
        case unAf af sz ar1 s1 of
          (# ar2, s2, (# a | | #) #) -> (# ar2, s2, (# f a | | #) #)
          (# ar2, s2, (# | e | #) #) -> (# ar2, s2, (# | e | #) #)
          (# ar2, s2, (# | | k #) #) -> (# ar2, s2, (# | | \ x -> fmapAf f (k x) #) #)
      (# ar1, s1, (# | e | #) #) ->
        (# ar1, s1, (# | e | #) #)
      (# ar1, s1, (# | | k #) #) ->
        (# ar1, s1, (# | | \ x -> apAf (k x) af #) #)


{-# NOINLINE bindAf #-}
bindAf :: forall efs a b. Af efs a -> (a -> Af efs b) -> Af efs b
bindAf mf ff = Af $ \ sz ar s -> do
  case unAf mf sz ar s of
    (# ar', s', (# a | | #) #) ->
      unAf (ff a) sz ar' s'
    (# ar', s', (# | e | #) #) ->
      (# ar', s', (# | e | #) #)
    (# ar', s', (# | | k #) #) ->
      (# ar', s', (# | | \ x -> bindAf (k x) ff #) #)


instance Monad (Af efs) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  mf >>= ff = Af $ \ sz ar s -> do
    case unAf mf sz ar s of
      (# ar', s', (# a | | #) #) ->
        unAf (ff a) sz ar' s'
      (# ar', s', (# | e | #) #) ->
        (# ar', s', (# | e | #) #)
      (# ar', s', (# | | k #) #) ->
        (# ar', s', (# | | \ x -> bindAf (k x) ff #) #)


{-# INLINE initialAfArray #-}
initialAfArray :: forall s. State# s -> (# State# s, AfArray s #)
initialAfArray s = newAfArray 2# s


{-# INLINE runAf# #-}
runAf# :: forall efs a s. Af efs a -> State# s -> (# State# s, a #)
runAf# af s0 =
  case initialAfArray s0 of
    (# s1, ar #) ->
      case unAf af (makeI16Pair 1# 0#) ar s1 of
        (# _, s2, (# a | | #) #) ->
          (# s2, a #)
        (# _, s2, (# | _ | #) #) ->
          (# s2, error "unhandled Af escape effect" #)
        (# _, s2, (# | | _ #) #) ->
          (# s2, error "unhandled Af backtrack effect" #)


{-# INLINE runAfPure #-}
runAfPure :: forall a. Af '[] a -> a
runAfPure af = case GHC.runRW# (runAf# af) of (# _, a #) -> a


{-# INLINE runAfHead #-}
runAfHead :: forall e efs a. Af (e : efs) a -> Af (Effects e ++ efs) a
runAfHead = unsafeCoerce
