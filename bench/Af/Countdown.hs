module Af.Countdown
  ( runCountdownPut
  , runCountdownExc
  ) where

import Control.Af
import Control.Af.State
import Control.Af.Error


{-# NOINLINE countdownPut #-}
countdownPut :: In (State Int) efs => Af efs Int
countdownPut = do
  n <- get @Int
  if n < 0
  then pure n
  else put (n - 1) *> countdownPut


{-# NOINLINE runCountdownPut #-}
runCountdownPut :: Int -> (Int, Int)
runCountdownPut n = runAfPure $ runState countdownPut n


{-# NOINLINE countdownExc #-}
countdownExc :: AllIn '[State Int, Error String] efs => Af efs Int
countdownExc = do
  n <- get @Int
  if n <= 0
  then throwError "what"
  else put (n - 1) *> countdownExc


{-# NOINLINE runCountdownExc #-}
runCountdownExc :: Int -> Either String (Int, Int)
runCountdownExc n = runAfPure $ runError (runState countdownExc n)