module Control.Af.Internal.Af
  ( Af (..)
  , AfCont (..)
  , runAfCont
  , runAfCont1
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


data AfCont :: [*] -> * -> [*] -> * -> * where
  AfContNil :: forall efs a. AfCont efs a efs a
  AfContBind ::
    forall dfs efs a b c. (b -> Af efs c) -> AfCont dfs a efs b -> AfCont dfs a efs c
  AfContScope ::
    forall cfs dfs efs a b c. (Af dfs b -> Af efs c) -> AfCont cfs a dfs b -> AfCont cfs a efs c


{-# NOINLINE runAfCont1 #-}
runAfCont1 :: forall efs dfs a b. AfCont efs a dfs b -> Af efs a -> Af dfs b
runAfCont1 AfContNil a = a
runAfCont1 (AfContBind f c) a = inline bindAf (runAfCont1 c a) f
runAfCont1 (AfContScope f c) a = f (runAfCont1 c a)


{-# NOINLINE runAfCont #-}
runAfCont :: forall efs dfs a b. AfCont efs a dfs b -> a -> Af dfs b
runAfCont AfContNil a = inline return a
runAfCont (AfContBind f AfContNil) a = f a
runAfCont (AfContBind f c) a = inline bindAf (runAfCont c a) f
runAfCont (AfContScope f c) a = f (runAfCont c a)


newtype Af (efs :: [*]) (a :: *) = Af
  { unAf ::
      forall dfs s.
      I16Pair -> AfArray s -> State# s ->
      (# AfArray s, State# s, (# a | (# Any | AfCont dfs Any efs a #) #) #)
  }


{-# INLINABLE fmapAf #-}
fmapAf :: forall efs a b. (a -> b) -> Af efs a -> Af efs b
fmapAf f af = Af $ \ sz ar s ->
  case unAf af sz ar s of
    (# ar', s', (# a | #) #) ->
      {-# SCC fmapAf_value #-}
      (# ar', s', (# f a | #) #)
    (# ar', s', (# | (# e | #) #) #) ->
      (# ar', s', (# | (# e | #) #) #)
    (# ar', s', (# | (# | k #) #) #) ->
      {-# SCC bindAf_AfContBind #-}
      (# ar', s', (# | (# | AfContBind (inline return . f) k #) #) #)


instance Functor (Af efs) where
  {-# INLINE fmap #-}
  fmap = fmapAf

  {-# INLINE (<$) #-}
  (<$) = fmapAf . const


instance Applicative (Af efs) where
  {-# INLINE pure #-}
  pure = inline return

  {-# INLINE (<*>) #-}
  ff <*> af = bindAf ff (\ f -> inline fmapAf f af)

  {-# INLINE (*>) #-}
  af1 *> af2 = bindAf af1 (\ _ -> af2)

  {-# INLINE (<*) #-}
  af1 <* af2 = bindAf af1 (<$ af2)


{-# INLINABLE bindAf #-}
bindAf :: forall efs a b. Af efs a -> (a -> Af efs b) -> Af efs b
bindAf af ff = Af $ \ sz ar s ->
  case unAf af sz ar s of
    (# ar', s', (# a | #) #) ->
      {-# SCC bindAf_value #-}
      unAf (ff a) sz ar' s'
    (# ar', s', (# | (# e | #) #) #) ->
      (# ar', s', (# | (# e | #) #) #)
    (# ar', s', (# | (# | k #) #) #) ->
      {-# SCC bindAf_AfContBind #-}
      (# ar', s', (# | (# | AfContBind ff k #) #) #)


instance Monad (Af efs) where
  {-# INLINE return #-}
  return a = Af $ \ _ ar s -> (# ar, s, (# a | #) #)

  {-# INLINE (>>=) #-}
  (>>=) = bindAf

  {-# INLINE (>>) #-}
  af1 >> af2 = bindAf af1 (\ _ -> af2)


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
