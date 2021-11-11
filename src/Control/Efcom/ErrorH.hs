module Control.Efcom.ErrorH
  ( Error
  , runError
  , throwError
  ) where


import Control.Efcom
import Control.Efcom.Handle


data Error e

newtype ErrorS (e :: *) (efs :: [*]) (a :: *) = ErrorS e

type instance Effect (Error e) = '[Handle (ErrorS e)]


{-# INLINE errorHandler #-}
errorHandler :: forall e efs a. Handler (ErrorS e) efs (Either e a)
errorHandler (ErrorS e) _ = return (Left e)


{-# INLINE runError #-}
runError :: forall e efs a. Com (Error e : efs) a -> Com efs (Either e a)
runError ef =
  runHandle @(Error e) (runComHead ef) Right errorHandler


{-# INLINE throwError #-}
throwError :: forall e efs a. In (Error e) efs => e -> Com efs a
throwError = backtrackHandle @(Error e) . ErrorS
