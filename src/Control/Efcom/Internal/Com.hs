module Control.Efcom.Internal.Com
  ( Com (..)
  , ComCont (..)
  , runComCont
  , runComCont1
  , runCom#
  , runComPure
  , runComHead
  ) where

import Control.Efcom.Internal.I16Pair
import Control.Efcom.Internal.Effect
import Control.Efcom.Internal.ComArray

import GHC.Exts (inline, Any, State#)
import qualified GHC.Exts as GHC

import Unsafe.Coerce (unsafeCoerce)


data ComCont :: [*] -> * -> [*] -> * -> * where
  ComContBind ::
    forall dfs efs a b c. (b -> Com efs c) -> ComCont dfs a efs b -> ComCont dfs a efs c
  ComContFmap ::
    forall dfs efs a b c. (b -> c) -> ComCont dfs a efs b -> ComCont dfs a efs c
  ComContScope ::
    forall cfs dfs efs a b c. (Com dfs b -> Com efs c) -> ComCont cfs a dfs b -> ComCont cfs a efs c
  ComContNil :: forall efs a. ComCont efs a efs a


{-# NOINLINE runComCont1 #-}
runComCont1 :: forall efs dfs a b. ComCont efs a dfs b -> Com efs a -> Com dfs b
runComCont1 ComContNil a = a
runComCont1 (ComContBind f c) a = inline bindCom (runComCont1 c a) f
runComCont1 (ComContFmap f c) a = inline fmapCom f (runComCont1 c a)
runComCont1 (ComContScope f c) a = f (runComCont1 c a)


{-# NOINLINE runComCont #-}
runComCont :: forall efs dfs a b. ComCont efs a dfs b -> a -> Com dfs b
runComCont ComContNil a = inline return a
runComCont (ComContBind f ComContNil) a = f a
runComCont (ComContBind f c) a = inline bindCom (runComCont c a) f
runComCont (ComContFmap f ComContNil) a = return (f a)
runComCont (ComContFmap f c) a = inline fmapCom f (runComCont c a)
runComCont (ComContScope f c) a = f (runComCont c a)


newtype Com (efs :: [*]) (a :: *) = Com
  { unCom ::
      forall dfs s.
      State# s -> I16Pair -> ComArray s ->
      (# ComArray s, State# s, (# a | (# Any | ComCont dfs Any efs a #) #) #)
  }


{-# INLINABLE fmapCom #-}
fmapCom :: forall efs a b. (a -> b) -> Com efs a -> Com efs b
fmapCom f ef = Com $ \ s0 sz ar ->
  case unCom ef s0 sz ar of
    (# ar', s1, (# a | #) #) ->
      {-# SCC fmapCom_value #-}
      (# ar', s1, (# f a | #) #)
    (# ar', s1, (# | (# e | #) #) #) ->
      (# ar', s1, (# | (# e | #) #) #)
    (# ar', s1, (# | (# | k #) #) #) ->
      {-# SCC fmapCom_ComContBind #-}
      (# ar', s1, (# | (# | ComContFmap f k #) #) #)


instance Functor (Com efs) where
  {-# INLINE fmap #-}
  fmap = fmapCom

  {-# INLINE (<$) #-}
  (<$) = fmapCom . const


instance Applicative (Com efs) where
  {-# INLINE pure #-}
  pure = inline return

  {-# INLINE (<*>) #-}
  ff <*> ef = bindCom ff (\ f -> inline fmapCom f ef)

  {-# INLINE (*>) #-}
  af1 *> af2 = bindCom af1 (\ _ -> af2)

  {-# INLINE (<*) #-}
  af1 <* af2 = bindCom af1 (<$ af2)


{-# INLINABLE bindCom #-}
bindCom :: forall efs a b. Com efs a -> (a -> Com efs b) -> Com efs b
bindCom ef ff = Com $ \ s0 sz ar ->
  case unCom ef s0 sz ar of
    (# ar', s1, (# a | #) #) ->
      {-# SCC bindCom_value #-}
      unCom (ff a) s1 sz ar'
    (# ar', s1, (# | (# e | #) #) #) ->
      (# ar', s1, (# | (# e | #) #) #)
    (# ar', s1, (# | (# | k #) #) #) ->
      {-# SCC bindCom_ComContBind #-}
      (# ar', s1, (# | (# | ComContBind ff k #) #) #)


instance Monad (Com efs) where
  {-# INLINE return #-}
  return a = Com $ \ s _ ar -> (# ar, s, (# a | #) #)

  {-# INLINE (>>=) #-}
  (>>=) = bindCom

  {-# INLINE (>>) #-}
  af1 >> af2 = bindCom af1 (\ _ -> af2)


{-# INLINE initialComArray #-}
initialComArray :: forall s. State# s -> (# State# s, ComArray s #)
initialComArray s = newComArray 4# s


{-# INLINE runCom# #-}
runCom# :: forall efs a s. Com efs a -> State# s -> (# State# s, a #)
runCom# ef s0 =
  case initialComArray s0 of
    (# s1, ar #) ->
      case unCom ef s1 (makeI16Pair 2# 0#) ar of
        (# _, s2, (# a | #) #) ->
          (# s2, a #)
        (# _, s2, (# | _ #) #) ->
          (# s2, error "unhandled Com (escape/backtracking) effect" #)


{-# INLINE runComPure #-}
runComPure :: forall a. Com '[] a -> a
runComPure ef = case GHC.runRW# (runCom# ef) of (# _, a #) -> a


{-# INLINE runComHead #-}
runComHead :: forall e efs a. Com (e : efs) a -> Com (Effects e ++ efs) a
runComHead = unsafeCoerce


-- This is probably not a good idea.
-- Using this inside a handler and applying continuation
-- can cause effect overflow if the continuation invokes handler
-- recursively.
{-# INLINE _liftCom #-}
_liftCom :: forall e efs a. Com efs a -> Com (e : efs) a
_liftCom = unsafeCoerce
