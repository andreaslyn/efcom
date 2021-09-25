module Af.Countdown
  ( runCountdownPut
  , runCountdownExc
  , runCountdownCountupExc
  ) where

import Control.Af
import Control.Af.State
import Control.Af.Error


{-# NOINLINE countdownPut #-}
countdownPut :: In (State Integer) efs => Af efs Integer
countdownPut = do
  n <- get @Integer
  if n < 0
  then pure n
  else put (n - 1) *> countdownPut


{-# NOINLINE runCountdownPut #-}
runCountdownPut :: Integer -> (Integer, Integer)
runCountdownPut n = runAfPure $ runState countdownPut n


{-# NOINLINE countdownExc #-}
countdownExc :: AllIn '[State Integer, Error String] efs => Af efs Integer
countdownExc = do
  n <- get @Integer
  if n <= 0
  then throwError "what"
  else put (n - 1) *> countdownExc


{-# NOINLINE runCountdownExc #-}
runCountdownExc :: Integer -> Either String (Integer, Integer)
runCountdownExc n = runAfPure $ runError (runState countdownExc n)


{-# NOINLINE countdownCountupExc #-}
countdownCountupExc :: AllIn '[State Integer, Error String] efs => Af efs Integer
countdownCountupExc = do
  n <- get @Integer
  if n <= 0
  then throwError "what"
  else put (n - 1) *> fmap (+1) countdownCountupExc


{-# NOINLINE runCountdownCountupExc #-}
runCountdownCountupExc :: Integer -> Either String (Integer, Integer)
runCountdownCountupExc n = runAfPure $ runError (runState countdownCountupExc n)
