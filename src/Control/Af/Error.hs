module Control.Af.Error
  ( Error
  , runError
  , throwError
  , catchError
  ) where

import Control.Af
import Control.Af.Shortcut


data Error e

type instance Effect (Error e) = '[Shortcut e]


{-# INLINE runError #-}
runError :: forall e efs a. Af (Error e : efs) a -> Af efs (Either e a)
runError af =
  runShortcut @(Error e) (meetEffect af) (return . Right) (return . Left)


{-# INLINE throwError #-}
throwError :: forall e efs a. In (Error e) efs => e -> Af efs a
throwError = takeShortcut @(Error e)


{-# INLINE catchError #-}
catchError ::
  forall e efs a. In (Error e) efs =>
  Af efs a -> (e -> Af efs a) -> Af efs a
catchError af h = scopeShortcut @(Error e) af return h (return ())
