module Control.Efcom.State
  ( State
  , runState
  , evalState
  , execState
  , get
  , put
  , lazyPut
  , modify
  , lazyModify
  , transaction
  , lazyTransaction
  ) where

import Control.Efcom
import Control.Efcom.Cell


data State s

type instance Effect (State s) = '[Cell s]


{-# INLINE runState #-}
runState :: forall s efs a. Com (State s : efs) a -> s -> Com efs (a, s)
runState ef s =
  runCell @(State s) (runComHead ef) s (\ a s' -> return (a, s'))


{-# INLINE evalState #-}
evalState :: forall s efs a. Com (State s : efs) a -> s -> Com efs a
evalState ef s =
  runCell @(State s) (runComHead ef) s (\ a _ -> return a)


{-# INLINE execState #-}
execState :: forall s efs a. Com (State s : efs) a -> s -> Com efs s
execState ef s =
  runCell @(State s) (runComHead ef) s (\ _ s' -> return s')


{-# INLINE get #-}
get :: forall s efs. In (State s) efs => Com efs s
get = readCell @(State s)


{-# INLINE put #-}
put :: forall s efs. In (State s) efs => s -> Com efs ()
put = writeCell @(State s)


{-# INLINE lazyPut #-}
lazyPut :: forall s efs. In (State s) efs => s -> Com efs ()
lazyPut = lazyWriteCell @(State s)


{-# INLINE modify #-}
modify :: forall s efs. In (State s) efs => (s -> s) -> Com efs ()
modify f = get >>= put . f


{-# INLINE lazyModify #-}
lazyModify :: forall s efs. In (State s) efs => (s -> s) -> Com efs ()
lazyModify f = get >>= lazyPut . f


{-# INLINE transaction #-}
transaction ::
  forall s efs a. In (State s) efs => Com efs a -> s -> Com efs a
transaction ef s =
  localCell @(State s) ef s (\ a s' -> put s' >> return a)


{-# INLINE lazyTransaction #-}
lazyTransaction ::
  forall s efs a. In (State s) efs => Com efs a -> s -> Com efs a
lazyTransaction ef s =
  localCell @(State s) ef s (\ a s' -> lazyPut s' >> return a)
