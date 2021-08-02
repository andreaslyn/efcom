module Control.Af.Reader
  ( Reader
  , runReader
  , ask
  , reader
  , local
  ) where

import Control.Af
import Control.Af.Cell


data Reader r

type instance Effect (Reader r) = '[Cell r]


{-# INLINE runReader #-}
runReader :: forall r efs a. Af (Reader r : efs) a -> r -> Af efs a
runReader af r =
  runCell @(Reader r) (runAfHead af) r (\a _ -> return a)


{-# INLINE ask #-}
ask :: forall r efs. In (Reader r) efs => Af efs r
ask = readCell @(Reader r)


{-# INLINE reader #-}
reader :: forall r efs a. In (Reader r) efs => (r -> a) -> Af efs a
reader f = do
  r <- ask
  return (f r)


{-# INLINE local #-}
local ::
  forall r efs a. In (Reader r) efs =>
  (r -> r) -> Af efs a -> Af efs a
local f af = do
  r <- ask
  scopeCell @(Reader r) af (f r) (\a _ -> return a) (\_ -> return ())
