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
      forall s.
      I16Pair -> AfArray s -> State# s ->
      (# AfArray s, State# s, (# Any | a #) #)
  }


instance Functor (Af efs) where
  {-# INLINE fmap #-}
  fmap f af = Af $ \ sz ar s ->
    case unAf af sz ar s of
      (# ar', s', (# e | #) #) -> (# ar', s', (# e | #) #)
      (# ar', s', (# | a #) #) -> (# ar', s', (# | f a #) #)


instance Applicative (Af efs) where
  {-# INLINE pure #-}
  pure a = Af $ \ _ ar s -> (# ar, s, (# | a #) #)

  {-# INLINE (<*>) #-}
  ff <*> af = Af $ \ sz ar s ->
    case unAf ff sz ar s of
      (# ar1, s1, (# e | #) #) -> (# ar1, s1, (# e | #) #)
      (# ar1, s1, (# | f #) #) -> unAf (fmap f af) sz ar1 s1


instance Monad (Af efs) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  mf >>= ff = Af $ \ sz ar s -> do
    case unAf mf sz ar s of
      (# ar', s', (# e | #) #) -> (# ar', s', (# e | #) #)
      (# ar', s', (# | a #) #) -> unAf (ff a) sz ar' s'


{-# INLINE initialAfArray #-}
initialAfArray :: forall s. State# s -> (# State# s, AfArray s #)
initialAfArray s = newAfArray 2# s


{-# INLINE runAf# #-}
runAf# :: forall efs a s. Af efs a -> State# s -> (# State# s, a #)
runAf# af s0 =
  case initialAfArray s0 of
    (# s1, ar #) ->
      case unAf af (makeI16Pair 1# 0#) ar s1 of
        (# _, s2, (# _ | #) #) ->
          (# s2, error "unhandled Af escape effect" #)
        (# _, s2, (# | a #) #) ->
          (# s2, a #)


{-# INLINE runAfPure #-}
runAfPure :: forall a. Af '[] a -> a
runAfPure af = case GHC.runRW# (runAf# af) of (# _, a #) -> a


{-# INLINE runAfHead #-}
runAfHead :: forall e efs a. Af (e : efs) a -> Af (Effects e ++ efs) a
runAfHead = unsafeCoerce
