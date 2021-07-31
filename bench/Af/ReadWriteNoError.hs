module Af.ReadWriteNoError
  ( runReadWriteNoError
  , readWriteNoError
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
loop i = flip catchError (throwError @String) $ do
  x <- ask @Int
  tell (Sum x)
  void $ listen @(Sum Int) $ do
    when (i < 0) (throwError "unexpected error")
    local @Int (+0) (loop (i - 1))


numberIterations :: Int
numberIterations = 100000


{-# NOINLINE readWriteNoError #-}
readWriteNoError :: ReadWriteNoError efs => Af efs ()
readWriteNoError = loop numberIterations


type ReadWriteNoErrorT =
  Af '[State Int, Reader Int, Writer (Sum Int), Error String] ()


{-# NOINLINE runReadWriteNoError #-}
runReadWriteNoError :: ReadWriteNoErrorT -> Either String (Sum Int)
runReadWriteNoError comp =
  pureAf $ runError (execWriter (runReader (evalState comp 0) 1))
