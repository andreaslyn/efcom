module Control.Efcom.Reader
  ( Reader
  , runReader
  , ask
  , reader
  , local
  ) where

import Control.Efcom
import Control.Efcom.Cell


data Reader r

type instance Effect (Reader r) = '[Cell r]


{-# INLINE runReader #-}
runReader :: forall r efs a. Com (Reader r : efs) a -> r -> Com efs a
runReader ef r =
  runCell @(Reader r) (runComHead ef) r (\ a _ -> return a)


{-# INLINE ask #-}
ask :: forall r efs. In (Reader r) efs => Com efs r
ask = readCell @(Reader r)


{-# INLINE reader #-}
reader :: forall r efs a. In (Reader r) efs => (r -> a) -> Com efs a
reader f = do
  r <- ask
  return (f r)


{-# INLINE local #-}
local ::
  forall r efs a. In (Reader r) efs =>
  (r -> r) -> Com efs a -> Com efs a
local f ef = do
  r <- ask
  localCell @(Reader r) ef (f r) (\ a _ -> return a)
