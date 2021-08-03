module MTL.ReadWriteNoError
  ( runReadWriteNoError
  ) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.State


type ReadWriteNoErrorT a =
  StateT Int (ReaderT Int (WriterT (Sum Int) (Except String))) a


{-# INLINABLE loop #-}
loop :: Int -> ReadWriteNoErrorT ()
loop 0 = return ()
loop i = flip catchError throwError $ do
  x <- ask @Int
  tell $! Sum x
  void $ listen $ do
    when (i < 0) (throwError "unexpected error")
    local (+0) (loop (i - 1))


{-# INLINABLE runReadWriteNoError #-}
runReadWriteNoError :: Int -> Either String (Sum Int)
runReadWriteNoError n =
  runExcept $
  execWriterT $
  flip runReaderT 1 $
  runStateT (loop n) 0
