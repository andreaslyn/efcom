{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.ExceptE
  ( ExceptE
  , runExcept
  , raise
  , except
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Af
import Control.Af.Internal.In

import Unsafe.Coerce (unsafeCoerce)


{-# INLINE runExcept #-}
runExcept ::
  forall e ex es a b.
  Af (ExceptE ex e : es) a -> (a -> Af es b) -> (ex -> Af es b) -> Af es b
runExcept af f g = Af $ \ sz ar0 s0 ->
  case unAf af sz ar0 s0 of
    (# ar1, s1, (# e | #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 0
          then
            unAf (g (unsafeCoerce e)) sz ar1 s2
          else
            let s3 = writeStrictAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# e | #) #)
    (# ar1, s1, (# | a #) #) ->
      unAf (f a) sz ar1 s1


{-# INLINE raise #-}
raise :: forall e ex es a. In (ExceptE ex e) es => ex -> Af es a
raise ex = Af $ \ _ ar s ->
  let s' = writeStrictAfArray @Int ar 0# (afExDepth @(ExceptE ex e) @es) s
  in (# ar, s', (# unsafeCoerce ex | #) #)



{-# INLINE except #-}
except ::
  forall e ex es a b c. In (ExceptE ex e) es =>
  Af es a -> (a -> Af es b) -> (ex -> Af es b) -> Af es c -> Af es b
except af f g r = Af $ \ sz ar0 s0 ->
  let ix = afStIndex @(ExceptE ex e) @es sz in
  case copyFromAfArray ar0 ix sz s0 of
    (# s1, backup #) ->
      case unAf af sz ar0 s1 of
        (# ar1, s2, (# e | #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, i #) ->
              if i == afExDepth @(ExceptE ex e) @es
              then
                let s4 = copyToAfArray backup ar1 ix s3
                in unAf (g (unsafeCoerce e)) sz ar1 s4
              else
                let !(# ar2, s4, _ #) = unAf r sz ar1 s3
                in (# ar2, s4, (# e | #) #)
        (# ar1, s2, (# | a #) #) ->
          unAf (f a) sz ar1 s2
