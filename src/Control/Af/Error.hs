module Control.Af.Error
  ( Error
  , runError
  , throwError
  , catchError
  ) where

import Control.Af
import Control.Af.Escape


data Error e

type instance Effect (Error e) = '[Escape e]


{-# INLINE runError #-}
runError :: forall e efs a. Af (Error e : efs) a -> Af efs (Either e a)
runError af =
  runEscape @(Error e) (runAfHead af) (return . Right) (return . Left)


{-# INLINE throwError #-}
throwError :: forall e efs a. In (Error e) efs => e -> Af efs a
throwError = takeEscape @(Error e)


{-# INLINE catchError #-}
catchError ::
  forall e efs a. In (Error e) efs =>
  Af efs a -> (e -> Af efs a) -> Af efs a
catchError af h = scopeEscape @(Error e) af return h (return ())
