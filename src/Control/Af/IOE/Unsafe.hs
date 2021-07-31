{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Af.IOE.Unsafe
  ( IOE
  , withIOE
  , withIOERunSTE
  , AfEnvIO (..)
  , unsafeAfEnvIOError
  , unsafeAfEnvIOSuccess
  , AfToIO
  , unsafeAfToIO
  , unsafeControlIO
  , unsafeLiftIO
  , controlIO
  , liftIO
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Af
import Control.Af.Internal.In
import Control.Af.Internal.Util

import Control.Af.STE.Unsafe

import GHC.Exts
  ( Any
  , Int#
  )
import qualified GHC.Exts as GHC
import qualified GHC.IO as GHC

import Control.Monad.IO.Class (MonadIO (..))


{-# INLINE withIOE #-}
withIOE :: forall a. Af '[IOE] a -> IO a
withIOE af = GHC.IO (runAf# af)


{-# INLINE withIOERunSTE #-}
withIOERunSTE :: forall a. (forall st. Af '[STE st, IOE] a) -> IO a
withIOERunSTE af = GHC.IO (runAf# af)


newtype AfEnvIO a = AfEnvIO (AfEnv GHC.RealWorld a)


{-# INLINE unsafeAfEnvIOError #-}
unsafeAfEnvIOError :: forall s a. AfArray s -> Any -> AfEnvIO a
unsafeAfEnvIOError ar e = AfEnvIO (unsafeAfEnvError ar e)


{-# INLINE unsafeAfEnvIOSuccess #-}
unsafeAfEnvIOSuccess :: forall s a. AfArray s -> a -> AfEnvIO a
unsafeAfEnvIOSuccess ar a = AfEnvIO (unsafeAfEnvSuccess ar a)


type AfToIO es = forall a. Af es a -> IO (AfEnvIO a)


{-# INLINE unsafeAfToIO #-}
unsafeAfToIO :: forall es s. Int# -> AfArray s -> AfToIO es
unsafeAfToIO sz ar = \af -> GHC.IO $ \s ->
  case unAf af sz ar (unsafeCoerceState s) of
    (# ar', s', (# e | #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvIOError ar' e #)
    (# ar', s', (# | a #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvIOSuccess ar' a #)


{-# INLINE unsafeControlIO #-}
unsafeControlIO ::
  forall es a.
  (AfToIO es -> IO (AfEnvIO a)) -> Af es a
unsafeControlIO f = Af $ \ sz ar0 s0 ->
  case GHC.unIO (f (unsafeAfToIO sz ar0)) (unsafeCoerceState s0) of
    (# s1, AfEnvIO (AfEnvError ar1 e) #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# e | #) #)
    (# s1, AfEnvIO (AfEnvSuccess ar1 a) #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# | a #) #)


{-# INLINE unsafeLiftIO #-}
unsafeLiftIO :: forall es a. IO a -> Af es a
unsafeLiftIO io = Af $ \ _ ar s0 ->
  let !(# s1, a #) = GHC.unIO io (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# | a #) #)


{-# INLINE controlIO #-}
controlIO ::
  forall es a. In IOE es =>
  (AfToIO es -> IO (AfEnvIO a)) -> Af es a
controlIO = unsafeControlIO


instance In IOE es => MonadIO (Af es) where
  {-# INLINE liftIO #-}
  liftIO = unsafeLiftIO
