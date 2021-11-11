module Control.Efcom.STE.Unsafe
  ( STE
  , runSTE
  , withSTE
  , ComEnv (..)
  , unsafeComEnvError
  , unsafeComEnvSuccess
  , unsafeComEnvBacktrack
  , unsafeCoerceBacktrack
  , unST
  , ComToST
  , liftST
  ) where

import Control.Efcom.Internal.Effect
import Control.Efcom.Internal.ComArray
import Control.Efcom.Internal.Com
--import Control.Efcom.Internal.I16Pair
import Control.Efcom.Internal.In
import Control.Efcom.Internal.Util

import GHC.Exts (Any, State#)
import GHC.ST (ST (..), runST)

import qualified Unsafe.Coerce


{-# INLINE withSTE #-}
withSTE :: forall st a. Com '[STE st] a -> ST st a
withSTE ef = ST (runCom# ef)


{-# INLINE runSTE #-}
runSTE :: forall a. (forall st. Com '[STE st] a) -> a
runSTE ef = runST (withSTE ef)


{-# INLINE unsafeCoerceBacktrack #-}
unsafeCoerceBacktrack ::
  forall dfs dfs' efs efs' a.
  ComCont dfs Any efs a -> ComCont dfs' Any efs' a
unsafeCoerceBacktrack = Unsafe.Coerce.unsafeCoerce


data ComEnv s a =
    ComEnvError (ComArray s) Any
  | ComEnvSuccess (ComArray s) a
  | forall dfs efs. ComEnvBacktrack (ComArray s) (ComCont dfs Any efs a)


instance Functor (ComEnv s) where
  fmap _ (ComEnvError ar e) = ComEnvError ar e
  fmap f (ComEnvSuccess ar a) = ComEnvSuccess ar (f a)
  fmap _ (ComEnvBacktrack ar k) = Unsafe.Coerce.unsafeCoerce (ComEnvBacktrack ar k)


{-# INLINE unsafeComEnvError #-}
unsafeComEnvError :: forall s t a. ComArray s -> Any -> ComEnv t a
unsafeComEnvError ar e = ComEnvError (unsafeCoerceComArray ar) e


{-# INLINE unsafeComEnvSuccess #-}
unsafeComEnvSuccess :: forall s t a. ComArray s -> a -> ComEnv t a
unsafeComEnvSuccess ar a = ComEnvSuccess (unsafeCoerceComArray ar) a


{-# INLINE unsafeComEnvBacktrack #-}
unsafeComEnvBacktrack ::
  forall s t dfs efs a.
  ComArray s -> ComCont dfs Any efs a -> ComEnv t a
unsafeComEnvBacktrack ar k =
  ComEnvBacktrack (unsafeCoerceComArray ar) (unsafeCoerceBacktrack k)


{-# INLINE unST #-}
unST :: forall st a. ST st a -> State# st -> (# State# st, a #)
unST (ST f) = f


type ComToST st es = forall a. Com es a -> ST st (ComEnv st a)


{-# INLINE liftST #-}
liftST :: forall st es a. In (STE st) es => ST st a -> Com es a
liftST st = Com $ \ s0 _ ar ->
  let !(# s1, a #) = unST st (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# a | #) #)


-- TODO missing transST or similar, see IOE/Unsafe.hs.
