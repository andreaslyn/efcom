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
  , unsafeLiftIO
  , transIO
  , liftIO
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Af
import Control.Af.Internal.I16Pair
import Control.Af.Internal.In
import Control.Af.Internal.Util

import Control.Af.STE.Unsafe

import GHC.Exts (Any)
import qualified GHC.Exts as GHC
import qualified GHC.IO as GHC

import Control.Monad.IO.Class (MonadIO (..))

import Unsafe.Coerce (unsafeCoerce)


{-# INLINE withIOE #-}
withIOE :: forall a. Af '[IOE] a -> IO a
withIOE af = GHC.IO (runAf# af)


{-# INLINE withIOERunSTE #-}
withIOERunSTE :: forall a. (forall st. Af '[STE st, IOE] a) -> IO a
withIOERunSTE af = GHC.IO (runAf# af)


newtype AfEnvIO a = AfEnvIO (AfEnv GHC.RealWorld a)
  deriving Functor


{-# INLINE unsafeAfEnvIOError #-}
unsafeAfEnvIOError :: forall s a. AfArray s -> Any -> AfEnvIO a
unsafeAfEnvIOError ar e = AfEnvIO (unsafeAfEnvError ar e)


{-# INLINE unsafeAfEnvIOSuccess #-}
unsafeAfEnvIOSuccess :: forall s a. AfArray s -> a -> AfEnvIO a
unsafeAfEnvIOSuccess ar a = AfEnvIO (unsafeAfEnvSuccess ar a)


{-# INLINE unsafeAfEnvIOBacktrack #-}
unsafeAfEnvIOBacktrack ::
  forall s dfs efs a.
  AfArray s -> (Af dfs Any -> Af efs a) -> AfEnvIO a
unsafeAfEnvIOBacktrack ar k = AfEnvIO (unsafeAfEnvBacktrack ar k)


type AfToIO es = forall a. Af es a -> IO (AfEnvIO a)


{-# INLINE unsafeAfToIO #-}
unsafeAfToIO :: forall es s. I16Pair -> AfArray s -> AfToIO es
unsafeAfToIO sz ar = \ af -> GHC.IO $ \ s ->
  case unAf af sz ar (unsafeCoerceState s) of
    (# ar', s', (# a | #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvIOSuccess ar' a #)
    (# ar', s', (# | (# e | #) #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvIOError ar' e #)
    (# ar', s', (# | (# | k #) #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvIOBacktrack ar' k #)


{-# INLINE unsafeTransIO #-}
unsafeTransIO ::
  forall es a b.
  (AfEnvIO () -> IO (AfEnvIO a) -> IO (AfEnvIO b)) ->
  Af es a -> Af es b
unsafeTransIO f af = Af $ \ sz ar0 s0 ->
  case GHC.unIO (f (unsafeAfEnvIOSuccess ar0 ()) (unsafeAfToIO sz ar0 af)) (unsafeCoerceState s0) of
    (# s1, AfEnvIO (AfEnvSuccess ar1 a) #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# a | #) #)
    (# s1, AfEnvIO (AfEnvError ar1 e) #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# | (# e | #) #) #)
    (# s1, AfEnvIO (AfEnvBacktrack ar1 k) #) ->
      (# unsafeCoerceAfArray ar1
       , unsafeCoerceState s1
       , (# | (# | unsafeCoerceBacktrack (unsafeTransIO f . unsafeCoerce k) #) #) #)


{-# INLINE unsafeLiftIO #-}
unsafeLiftIO :: forall es a. IO a -> Af es a
unsafeLiftIO io = Af $ \ _ ar s0 ->
  let !(# s1, a #) = GHC.unIO io (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# a | #) #)


-- THIS IS NOT SAFE.
-- IT INVOKES THE OPERATION TOO OFTEN WHEN BACKTRACKING.
-- IT IS SAFE FOR PURE/ALGEBRAIC/REENTRANT FUNCTIONS, SUCH AS try.
{-# INLINE transIO #-}
transIO ::
  forall es a b. In IOE es =>
  (AfEnvIO () -> IO (AfEnvIO a) -> IO (AfEnvIO b)) ->
  Af es a -> Af es b
transIO = unsafeTransIO


instance In IOE es => MonadIO (Af es) where
  {-# INLINE liftIO #-}
  liftIO = unsafeLiftIO
