module Control.Af.State
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

import Control.Af
import Control.Af.Cell


data State s

type instance Effect (State s) = '[Cell s]


{-# INLINE runState #-}
runState :: forall s efs a. Af (State s : efs) a -> s -> Af efs (a, s)
runState af s =
  runCell @(State s) (runAfHead af) s (\ a s' -> return (a, s'))


{-# INLINE evalState #-}
evalState :: forall s efs a. Af (State s : efs) a -> s -> Af efs a
evalState af s =
  runCell @(State s) (runAfHead af) s (\ a _ -> return a)


{-# INLINE execState #-}
execState :: forall s efs a. Af (State s : efs) a -> s -> Af efs s
execState af s =
  runCell @(State s) (runAfHead af) s (\ _ s' -> return s')


{-# INLINE get #-}
get :: forall s efs. In (State s) efs => Af efs s
get = readCell @(State s)


{-# INLINE put #-}
put :: forall s efs. In (State s) efs => s -> Af efs ()
put = writeCell @(State s)


{-# INLINE lazyPut #-}
lazyPut :: forall s efs. In (State s) efs => s -> Af efs ()
lazyPut = lazyWriteCell @(State s)


{-# INLINE modify #-}
modify :: forall s efs. In (State s) efs => (s -> s) -> Af efs ()
modify f = get >>= put . f


{-# INLINE lazyModify #-}
lazyModify :: forall s efs. In (State s) efs => (s -> s) -> Af efs ()
lazyModify f = get >>= lazyPut . f


{-# INLINE transaction #-}
transaction ::
  forall s efs a. In (State s) efs => Af efs a -> s -> Af efs a
transaction af s =
  localCell @(State s) af s (\ a s' -> put s' >> return a)


{-# INLINE lazyTransaction #-}
lazyTransaction ::
  forall s efs a. In (State s) efs => Af efs a -> s -> Af efs a
lazyTransaction af s =
  localCell @(State s) af s (\ a s' -> lazyPut s' >> return a)
