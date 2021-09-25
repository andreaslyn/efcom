module Control.Af.Internal.Af
  ( Af (..)
  , runAf#
  , runAfPure
  , runAfHead
  ) where

import Control.Af.Internal.I16Pair
import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray

import GHC.Exts (inline, Any, State#)
import qualified GHC.Exts as GHC

import Unsafe.Coerce (unsafeCoerce)


newtype Af (efs :: [*]) (a :: *) = Af
  { unAf ::
      forall dfs s.
      I16Pair -> AfArray s -> State# s ->
      (# AfArray s
       , State# s
       , (# a | (# Any | Af dfs Any -> Af efs a #) #) #)
  }


{-# NOINLINE fmapAfCont #-}
fmapAfCont ::
  forall dfs efs a b.
  (Af dfs Any -> Af efs a) -> (a -> b) ->
  Af dfs Any -> Af efs b
fmapAfCont k f x = inline fmapAf f (k x)


{-# INLINABLE fmapAf #-}
fmapAf :: forall efs a b. (a -> b) -> Af efs a -> Af efs b
fmapAf f af = Af $ \ sz ar s ->
  case unAf af sz ar s of
    (# ar', s', (# a | #) #) ->
      (# ar', s', (# f a | #) #)
    (# ar', s', (# | (# e | #) #) #) ->
      (# ar', s', (# | (# e | #) #) #)
    (# ar', s', (# | (# | k #) #) #) ->
      (# ar', s', (# | (# | fmapAfCont k f #) #) #)


instance Functor (Af efs) where
  {-# INLINE fmap #-}
  fmap = inline fmapAf

  {-# INLINE (<$) #-}
  (<$) = inline fmapAf . const


{-# NOINLINE apAfCont #-}
apAfCont ::
  forall dfs efs a b.
  (Af dfs Any -> Af efs (a -> b)) -> Af efs a ->
  Af dfs Any -> Af efs b
apAfCont k af x = inline apAf (k x) af


{-# INLINABLE apAf #-}
apAf :: forall efs a b. Af efs (a -> b) -> Af efs a -> Af efs b
apAf ff af = Af $ \ sz ar s ->
  case unAf ff sz ar s of
    (# ar1, s1, (# f | #) #) ->
        unAf (inline fmapAf f af) sz ar1 s1
    (# ar1, s1, (# | (# e | #) #) #) ->
      (# ar1, s1, (# | (# e | #) #) #)
    (# ar1, s1, (# | (# | k #) #) #) ->
      (# ar1, s1, (# | (# | apAfCont k af #) #) #)


instance Applicative (Af efs) where
  {-# INLINE pure #-}
  pure a = Af $ \ _ ar s -> (# ar, s, (# a | #) #)

  {-# INLINE (<*>) #-}
  (<*>) = inline apAf

  {-# INLINE (*>) #-}
  af1 *> af2 = inline bindAf af1 (\ _ -> af2)

  {-# INLINE (<*) #-}
  af1 <* af2 = inline bindAf af1 (<$ af2)


{-# NOINLINE bindAfCont #-}
bindAfCont ::
  forall dfs efs a b.
  (Af dfs Any -> Af efs a) -> (a -> Af efs b) ->
  Af dfs Any -> Af efs b
bindAfCont k ff x = inline bindAf (k x) ff


{-# INLINABLE bindAf #-}
bindAf :: forall efs a b. Af efs a -> (a -> Af efs b) -> Af efs b
bindAf mf ff = Af $ \ sz ar s ->
  case unAf mf sz ar s of
    (# ar', s', (# a | #) #) ->
      unAf (ff a) sz ar' s'
    (# ar', s', (# | (# e | #) #) #) ->
      (# ar', s', (# | (# e | #) #) #)
    (# ar', s', (# | (# | k #) #) #) ->
      (# ar', s', (# | (# | bindAfCont k ff #) #) #)


instance Monad (Af efs) where
  {-# INLINE return #-}
  return = inline pure

  {-# INLINE (>>=) #-}
  (>>=) = inline bindAf

  {-# INLINE (>>) #-}
  af1 >> af2 = inline bindAf af1 (\ _ -> af2)


{-# INLINE initialAfArray #-}
initialAfArray :: forall s. State# s -> (# State# s, AfArray s #)
initialAfArray s = newAfArray 4# s


{-# INLINE runAf# #-}
runAf# :: forall efs a s. Af efs a -> State# s -> (# State# s, a #)
runAf# af s0 =
  case initialAfArray s0 of
    (# s1, ar #) ->
      case unAf af (makeI16Pair 2# 0#) ar s1 of
        (# _, s2, (# a | #) #) ->
          (# s2, a #)
        (# _, s2, (# | _ #) #) ->
          (# s2, error "unhandled Af (escape/backtracking) effect" #)


{-# INLINE runAfPure #-}
runAfPure :: forall a. Af '[] a -> a
runAfPure af = case GHC.runRW# (runAf# af) of (# _, a #) -> a


{-# INLINE runAfHead #-}
runAfHead :: forall e efs a. Af (e : efs) a -> Af (Effects e ++ efs) a
runAfHead = unsafeCoerce
