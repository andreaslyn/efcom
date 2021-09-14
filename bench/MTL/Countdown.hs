module MTL.Countdown
  ( runCountdownPut
  , runCountdownExc
  , runCountdownCountupExc
  ) where

import Control.Monad.State.Strict
import Control.Monad.Except


{-# INLINABLE countdownPut #-}
countdownPut :: State Int Int
countdownPut = do
  n <- get @Int
  if n < 0
  then pure n
  else (put $! n - 1) *> countdownPut


{-# INLINABLE runCountdownPut #-}
runCountdownPut :: Int -> (Int, Int)
runCountdownPut n = runState countdownPut n


{-# NOINLINE countdownExc #-}
countdownExc :: StateT Int (Except String) Int
countdownExc = do
  n <- get @Int
  if n <= 0
  then throwError "what"
  else (put $! n - 1) *> countdownExc


{-# NOINLINE runCountdownExc #-}
runCountdownExc :: Int -> Either String (Int, Int)
runCountdownExc n = runExcept (runStateT countdownExc n)


{-# NOINLINE countdownCountupExc #-}
countdownCountupExc :: StateT Int (Except String) Int
countdownCountupExc = do
  n <- get @Int
  if n <= 0
  then throwError "what"
  else (put $! n - 1) *> fmap (+1) countdownCountupExc


{-# NOINLINE runCountdownCountupExc #-}
runCountdownCountupExc :: Int -> Either String (Int, Int)
runCountdownCountupExc n = runExcept (runStateT countdownCountupExc n)
