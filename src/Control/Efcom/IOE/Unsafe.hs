{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Efcom.IOE.Unsafe
  ( IOE
  , withIOE
  , withIOERunSTE
  , ComEnvIO (..)
  , unsafeComEnvIOError
  , unsafeComEnvIOSuccess
  , ComToIO
  , unsafeComToIO
  , unsafeLiftIO
  , transIO
  , liftIO
  ) where

import Control.Efcom.Internal.Effect
import Control.Efcom.Internal.ComArray
import Control.Efcom.Internal.Com
import Control.Efcom.Internal.I16Pair
import Control.Efcom.Internal.In
import Control.Efcom.Internal.Util

import Control.Efcom.STE.Unsafe

import GHC.Exts (Any)
import qualified GHC.Exts as GHC
import qualified GHC.IO as GHC

import Control.Monad.IO.Class (MonadIO (..))

import Unsafe.Coerce (unsafeCoerce)


{-# INLINE withIOE #-}
withIOE :: forall a. Com '[IOE] a -> IO a
withIOE ef = GHC.IO (runCom# ef)


{-# INLINE withIOERunSTE #-}
withIOERunSTE :: forall a. (forall st. Com '[STE st, IOE] a) -> IO a
withIOERunSTE ef = GHC.IO (runCom# ef)


newtype ComEnvIO a = ComEnvIO (ComEnv GHC.RealWorld a)
  deriving Functor


{-# INLINE unsafeComEnvIOError #-}
unsafeComEnvIOError :: forall s a. ComArray s -> Any -> ComEnvIO a
unsafeComEnvIOError ar e = ComEnvIO (unsafeComEnvError ar e)


{-# INLINE unsafeComEnvIOSuccess #-}
unsafeComEnvIOSuccess :: forall s a. ComArray s -> a -> ComEnvIO a
unsafeComEnvIOSuccess ar a = ComEnvIO (unsafeComEnvSuccess ar a)


{-# INLINE unsafeComEnvIOBacktrack #-}
unsafeComEnvIOBacktrack ::
  forall s dfs efs a.
  ComArray s -> ComCont dfs Any efs a -> ComEnvIO a
unsafeComEnvIOBacktrack ar k = ComEnvIO (unsafeComEnvBacktrack ar k)


type ComToIO efs = forall a. Com efs a -> IO (ComEnvIO a)


{-# INLINE unsafeComToIO #-}
unsafeComToIO :: forall efs s. I16Pair -> ComArray s -> ComToIO efs
unsafeComToIO sz ar = \ ef -> GHC.IO $ \ s ->
  case unCom ef (unsafeCoerceState s) sz ar of
    (# ar', s', (# a | #) #) ->
      (# unsafeCoerceState s', unsafeComEnvIOSuccess ar' a #)
    (# ar', s', (# | (# e | #) #) #) ->
      (# unsafeCoerceState s', unsafeComEnvIOError ar' e #)
    (# ar', s', (# | (# | k #) #) #) ->
      (# unsafeCoerceState s', unsafeComEnvIOBacktrack ar' k #)


{-# INLINE unsafeTransIO #-}
unsafeTransIO ::
  forall efs a b.
  (ComEnvIO () -> IO (ComEnvIO a) -> IO (ComEnvIO b)) ->
  Com efs a -> Com efs b
unsafeTransIO f ef = Com $ \ s0 sz ar0 ->
  case GHC.unIO (f (unsafeComEnvIOSuccess ar0 ()) (unsafeComToIO sz ar0 ef)) (unsafeCoerceState s0) of
    (# s1, ComEnvIO (ComEnvSuccess ar1 a) #) ->
      (# unsafeCoerceComArray ar1, unsafeCoerceState s1, (# a | #) #)
    (# s1, ComEnvIO (ComEnvError ar1 e) #) ->
      (# unsafeCoerceComArray ar1, unsafeCoerceState s1, (# | (# e | #) #) #)
    (# s1, ComEnvIO (ComEnvBacktrack ar1 k) #) ->
      (# unsafeCoerceComArray ar1
       , unsafeCoerceState s1
       , (# | (# | ComContScope (unsafeTransIO f) (unsafeCoerce k) #) #) #)


{-# INLINE unsafeLiftIO #-}
unsafeLiftIO :: forall efs a. IO a -> Com efs a
unsafeLiftIO io = Com $ \ s0 _ ar ->
  let !(# s1, a #) = GHC.unIO io (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# a | #) #)


-- THIS IS NOT SAFE.
-- IT INVOKES THE OPERATION TOO OFTEN WHEN BACKTRACKING.
-- IT IS SAFE FOR PURE/ALGEBRAIC/REENTRANT FUNCTIONS, SUCH AS try.
{-# INLINE transIO #-}
transIO ::
  forall efs a b. In IOE efs =>
  (ComEnvIO () -> IO (ComEnvIO a) -> IO (ComEnvIO b)) ->
  Com efs a -> Com efs b
transIO = unsafeTransIO


instance In IOE efs => MonadIO (Com efs) where
  {-# INLINE liftIO #-}
  liftIO = unsafeLiftIO
