{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Escape
  ( Escape
  , runEscape
  , takeEscape
  , scopeEscape
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Util
import Control.Af.Internal.I16Pair
import Control.Af.Internal.Af
import Control.Af.Internal.In

import Unsafe.Coerce (unsafeCoerce)

import qualified GHC.Exts as GHC


{-# INLINE doRunEscape #-}
doRunEscape ::
  forall ref ex efs a b.
  Af (Escape ex ref : efs) a -> -- Effectful computation to execute.
  (a -> Af efs b) ->        -- Action used if computation returns
                            -- without taking the shortcut.
  (ex -> Af efs b) ->       -- Action used if shortcut is taken.
  Af efs b
doRunEscape af f g = Af $ \ sz ar0 s0 ->
  case unAf af (addToSndI16Pair sz 1#) ar0 s0 of
    (# ar1, s1, (# e | #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 1
          then
            unAf (g (unsafeCoerce e)) sz ar1 s2
          else
            let s3 = writeStrictAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# e | #) #)
    (# ar1, s1, (# | a #) #) ->
      unAf (f a) sz ar1 s1


{-# INLINE runEscape #-}
runEscape ::
  forall ref ex efs a b.
  Af (Escape ex ref : efs) a -> -- Effectful computation to execute.
  (a -> Af efs b) ->        -- Action used if computation returns
                            -- without taking the shortcut.
  (ex -> Af efs b) ->       -- Action used if shortcut is taken.
  Af efs b
runEscape af f g = Af $ \ sz ar s ->
  case isMaxI16PairValue (sndI16Pair sz) of
    1# -> error "exceeded maximal number of escape effects"
    _ -> unAf (doRunEscape af f g) sz ar s


{-# INLINE takeEscape #-}
takeEscape :: forall ref ex efs a. In (Escape ex ref) efs => ex -> Af efs a
takeEscape ex = Af $ \ sz ar s ->
  let di = escapeDepthCellIndex @(Escape ex ref) @efs sz
      s' = writeStrictAfArray @Int ar 0# (GHC.I# (escapeDepth di)) s
  in (# ar, s', (# unsafeCoerce ex | #) #)



{-# INLINE scopeEscape #-}
scopeEscape ::
  forall ref ex efs a b. In (Escape ex ref) efs =>
  Af efs a ->           -- Effect computation to execute in scope.
  (a -> Af efs b) ->    -- Action when the shortcut is not taken.
  (ex -> Af efs b) ->   -- Action when the shortcut is taken.
  Af efs () ->          -- Action used when a shortcut of another
                        -- shortcut effect is taken.
  Af efs b
scopeEscape af f g r = Af $ \ sz ar0 s0 ->
  let di = escapeDepthCellIndex @(Escape ex ref) @efs sz in
  case copyFromAfArray ar0 (cellIndex di) (fstI16Pair sz) s0 of
    (# s1, backup #) ->
      case unAf af sz ar0 s1 of
        (# ar1, s2, (# e | #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, d #) ->
              case unI# d GHC.==# escapeDepth di of
                1# ->
                  let s4 = copyToAfArray backup ar1 (cellIndex di) s3
                  in unAf (g (unsafeCoerce e)) sz ar1 s4
                _ ->
                  case unI# d GHC.<# escapeDepth di of
                    1# -> -- Internal escape.
                      let !(# ar2, s4, _ #) = unAf r sz ar1 s3
                      in (# ar2, s4, (# e | #) #)
                    _ ->  -- External escape.
                      (# ar1, s3, (# e | #) #)
        (# ar1, s2, (# | a #) #) ->
          unAf (f a) sz ar1 s2
