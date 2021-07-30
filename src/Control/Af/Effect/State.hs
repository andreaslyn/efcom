{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Effect.State
  ( StateE
  , runState
  , local
  , get
  , put
  ) where

import Control.Af.Effect
import Control.Af.Af
import Control.Af.In


{-# INLINE runState #-}
runState ::
  forall e st es a b.
  Af (StateE st e : es) a -> st -> (a -> st -> Af es b) -> Af es b
runState af st k = Af $ \ sz ar0 s0 ->
  case appendAfArray sz ar0 st s0 of
    (# ar1, s1, sz' #) ->
      case unAf af sz' ar1 s1 of
        (# ar2, s2, (# e | #) #) ->
          (# ar2, writeAfArray ar2 sz undefinedAfElement s2, (# e | #) #)
        (# ar2, s2, (# | a #) #) ->
          case readAfArray ar2 sz s2 of
            (# s3, st' #) ->
              let s4 = writeAfArray ar2 sz undefinedAfElement s3
              in unAf (k a st') sz ar2 s4


{-# INLINE local #-}
local ::
  forall e st es a b c. In (StateE st e) es =>
  Af es a -> st -> (a -> st -> Af es b) -> (st -> Af es c) -> Af es b
local af st k g = Af $ \ sz ar0 s0 ->
  let ix = afStIndex @(StateE st e) @es sz in
  case readAfArray ar0 ix s0 of
    (# s1, orig #) ->
      let s2 = writeAfArray ar0 ix st s1 in
      case unAf af sz ar0 s2 of
        (# ar1, s3, (# e | #) #) ->
          case readAfArray ar1 ix s3 of
            (# s4, st' #) ->
              let s5 = writeAfArray ar1 ix orig s4
                  !(# ar2, s6, _ #) = unAf (g st') sz ar1 s5
              in (# ar2, s6, (# e | #) #)
        (# ar1, s3, (# | a #) #) ->
          case readAfArray ar1 ix s3 of
            (# s4, st' #) ->
              let s5 = writeAfArray ar1 ix orig s4
              in unAf (k a st') sz ar1 s5


{-# INLINE put #-}
put :: forall e st es. In (StateE st e) es => st -> Af es ()
put st = Af $ \ sz ar s ->
  let i = afStIndex @(StateE st e) @es sz
  in (# ar, writeAfArray ar i st s, (# | () #) #)


{-# INLINE get #-}
get :: forall e st es. In (StateE st e) es => Af es st
get = Af $ \ sz ar s ->
  let i = afStIndex @(StateE st e) @es sz
  in case readAfArray ar i s of
      (# s', a #) -> (# ar, s', (# | a #) #)


