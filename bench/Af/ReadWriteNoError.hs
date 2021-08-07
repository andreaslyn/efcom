module Af.ReadWriteNoError
  ( runReadWriteNoError
  ) where

import Control.Af
import Control.Af.Reader
import Control.Af.Writer
import Control.Af.Error
import Control.Af.State

import Data.Semigroup (Sum (..))
import Control.Monad (when, void)


type ReadWriteNoError efs =
  ( In (Reader Int) efs
  , In (Writer (Sum Int)) efs
  , In (Error String) efs
  )


{-# NOINLINE loop #-}
loop :: ReadWriteNoError efs => Int -> Af efs ()
loop 0 = return ()
loop i = catchError (do
    x <- ask @Int
    tell (Sum x)
    void $ listen @(Sum Int) $ do
      when (i < 0) (throwError "unexpected error")
      local @Int (+0) (loop (i - 1))
  ) (throwError @String)


{-# NOINLINE runReadWriteNoError #-}
runReadWriteNoError :: Int -> Either String (Sum Int)
runReadWriteNoError n =
  runAfPure $
  runError @String $
  execWriter @(Sum Int) $
  flip (runReader @Int) 1 $
  evalState @Int (loop n) 0
