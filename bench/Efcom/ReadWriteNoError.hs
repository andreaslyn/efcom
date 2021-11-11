module Efcom.ReadWriteNoError
  ( runReadWriteNoError
  ) where

import Control.Efcom
import Control.Efcom.Reader
import Control.Efcom.Writer
import Control.Efcom.Error
import Control.Efcom.State

import Data.Semigroup (Sum (..))
import Control.Monad (when, void)


type ReadWriteNoError efs =
  ( In (Reader Int) efs
  , In (Writer (Sum Int)) efs
  , In (Error String) efs
  )


{-# NOINLINE loop #-}
loop :: ReadWriteNoError efs => Int -> Com efs ()
loop 0 = return ()
loop i = flip catchError (throwError @String) $ do
  x <- ask @Int
  tell (Sum x)
  void $ listen @(Sum Int) $ do
    when (i < 0) (throwError "unexpected error")
    local @Int (+0) (loop (i - 1))


{-# NOINLINE runReadWriteNoError #-}
runReadWriteNoError :: Int -> Either String (Sum Int)
runReadWriteNoError n =
  runComPure $
  runError @String $
  execWriter @(Sum Int) $
  flip (runReader @Int) 1 $
  evalState @Int (loop n) 0
