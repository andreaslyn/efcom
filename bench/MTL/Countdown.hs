module MTL.Countdown
  ( runCountdownPut
  , runCountdownExc
  , runCountdownCountupExc
  ) where

import Control.Monad.State.Strict
import Control.Monad.Except


{-# NOINLINE countdownPut #-}
countdownPut :: State Integer Integer
countdownPut = do
  n <- get @Integer
  if n < 0
  then pure n
  else (put $! n - 1) *> countdownPut


{-# NOINLINE runCountdownPut #-}
runCountdownPut :: Integer -> (Integer, Integer)
runCountdownPut n = runState countdownPut n


{-# NOINLINE countdownExc #-}
countdownExc :: StateT Integer (Except String) Integer
countdownExc = do
  n <- get @Integer
  if n <= 0
  then throwError "what"
  else (put $! n - 1) *> countdownExc


{-# NOINLINE runCountdownExc #-}
runCountdownExc :: Integer -> Either String (Integer, Integer)
runCountdownExc n = runExcept (runStateT countdownExc n)


{-# NOINLINE countdownCountupExc #-}
countdownCountupExc :: StateT Integer (Except String) Integer
countdownCountupExc = do
  n <- get @Integer
  if n <= 0
  then throwError "what"
  else (put $! n - 1) *> fmap (+1) countdownCountupExc


{-# NOINLINE runCountdownCountupExc #-}
runCountdownCountupExc :: Integer -> Either String (Integer, Integer)
runCountdownCountupExc n = runExcept (runStateT countdownCountupExc n)
