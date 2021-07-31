module Control.Af.Internal.Af
  ( Af (..)
  , runAf#
  , pureAf
  , meetEffect
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray

import GHC.Exts
  ( Any
  , State#
  , Int#
  )
import qualified GHC.Exts as GHC

import Unsafe.Coerce (unsafeCoerce)


newtype Af (es :: [*]) (a :: *) =
  Af
  { unAf ::
      forall s.
      Int# -> AfArray s -> State# s ->
      (# AfArray s, State# s, (# Any | a #) #)
  }


instance Functor (Af es) where
  {-# INLINE fmap #-}
  fmap f af = Af $ \sz ar s ->
    case unAf af sz ar s of
      (# ar', s', (# e | #) #) -> (# ar', s', (# e | #) #)
      (# ar', s', (# | a #) #) -> (# ar', s', (# | f a #) #)


instance Applicative (Af es) where
  {-# INLINE pure #-}
  pure a = Af $ \ _ ar s -> (# ar, s, (# | a #) #)

  {-# INLINE (<*>) #-}
  ff <*> af = Af $ \ sz ar s ->
    case unAf ff sz ar s of
      (# ar1, s1, (# e | #) #) -> (# ar1, s1, (# e | #) #)
      (# ar1, s1, (# | f #) #) -> unAf (fmap f af) sz ar1 s1


instance Monad (Af es) where
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
runAf# :: forall es a s. Af es a -> State# s -> (# State# s, a #)
runAf# af s0 =
  case initialAfArray s0 of
    (# s1, ar #) ->
      case unAf af 1# ar s1 of
        (# _, s2, (# _ | #) #) ->
          (# s2, error "Af exception unexpectedly escaped" #)
        (# _, s2, (# | a #) #) ->
          (# s2, a #)


{-# INLINE pureAf #-}
pureAf :: forall a. Af '[] a -> a
pureAf af = case GHC.runRW# (runAf# af) of (# _, a #) -> a


{-# INLINE meetEffect #-}
meetEffect :: forall e es a. Af (e : es) a -> Af (MeetEffect e es) a
meetEffect = unsafeCoerce