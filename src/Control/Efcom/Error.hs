module Control.Efcom.Error
  ( Error
  , runError
  , throwError
  , catchError
  ) where

import Control.Efcom
import Control.Efcom.Escape


data Error e

type instance Effect (Error e) = '[Escape e]


{-# INLINE runError #-}
runError :: forall e efs a. Com (Error e : efs) a -> Com efs (Either e a)
runError ef =
  runEscape @(Error e) (runComHead ef) (return . Right) (return . Left)


{-# INLINE throwError #-}
throwError :: forall e efs a. In (Error e) efs => e -> Com efs a
throwError = takeEscape @(Error e)


{-# INLINE catchError #-}
catchError ::
  forall e efs a. In (Error e) efs =>
  Com efs a -> (e -> Com efs a) -> Com efs a
catchError = catchEscape @(Error e)
