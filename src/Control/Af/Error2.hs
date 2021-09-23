module Control.Af.Error2
  ( Error
  , runError
  , throwError
  ) where


import Control.Af
import Control.Af.Handle


data Error e

newtype ErrorS (e :: *) (efs :: [*]) (a :: *) = ErrorS e

type instance Effect (Error e) = '[Handle (ErrorS e)]


{-# INLINE errorHandler #-}
errorHandler :: forall e efs a. Handler (ErrorS e) efs (Either e a)
errorHandler (ErrorS e) _ = return (Left e)


{-# INLINE runError #-}
runError :: forall e efs a. Af (Error e : efs) a -> Af efs (Either e a)
runError af =
  runHandle @(Error e) (runAfHead af) Right errorHandler


{-# INLINE throwError #-}
throwError :: forall e efs a. In (Error e) efs => e -> Af efs a
throwError = backtrackHandle @(Error e) . ErrorS
