module Efcom.Countdown
  ( runCountdownPut
  , runCountdownExc
  , runCountdownCountupExc
  ) where

import Control.Efcom
import Control.Efcom.State
import Control.Efcom.Error


{-# NOINLINE countdownPut #-}
countdownPut :: In (State Integer) efs => Com efs Integer
countdownPut = do
  n <- get @Integer
  if n < 0
  then pure n
  else put (n - 1) *> countdownPut


{-# NOINLINE runCountdownPut #-}
runCountdownPut :: Integer -> (Integer, Integer)
runCountdownPut n = runComPure $ runState countdownPut n


{-# NOINLINE countdownExc #-}
countdownExc :: AllIn '[State Integer, Error String] efs => Com efs Integer
countdownExc = do
  n <- get @Integer
  if n <= 0
  then throwError "what"
  else put (n - 1) *> countdownExc


{-# NOINLINE runCountdownExc #-}
runCountdownExc :: Integer -> Either String (Integer, Integer)
runCountdownExc n = runComPure $ runError (runState countdownExc n)


{-# NOINLINE countdownCountupExc #-}
countdownCountupExc :: AllIn '[State Integer, Error String] efs => Com efs Integer
countdownCountupExc = do
  n <- get @Integer
  if n <= 0
  then throwError "what"
  else put (n - 1) *> fmap (+1) countdownCountupExc


{-# NOINLINE runCountdownCountupExc #-}
runCountdownCountupExc :: Integer -> Either String (Integer, Integer)
runCountdownCountupExc n = runComPure $ runError (runState countdownCountupExc n)
