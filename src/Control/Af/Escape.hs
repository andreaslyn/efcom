{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Escape
  ( Escape
  , runEscape
  , takeEscape
  , takeEscape_
  , delimitEscape
  , catchEscape
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Util
import Control.Af.Internal.I16Pair
import Control.Af.Internal.Af
import Control.Af.Internal.In

import Unsafe.Coerce (unsafeCoerce)

import GHC.Exts (inline)
import qualified GHC.Exts as GHC


{-# INLINE doRunEscape' #-}
doRunEscape' ::
  forall ref ex efs a b.
  Af (Escape ex ref : efs) a -> -- Effectful computation to execute.
  (a -> Af efs b) ->        -- Action used if computation returns
                            -- without taking the shortcut.
  (ex -> Af efs b) ->       -- Action used if shortcut is taken.
  Af efs b
doRunEscape' af f g = Af $ \ sz ar0 s0 ->
  case unAf af (addSndI16Pair sz 1#) ar0 s0 of
    (# ar1, s1, (# a | | #) #) ->
      unAf (f a) sz ar1 s1
    (# ar1, s1, (# | e | #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 1
          then
            unAf (g (unsafeCoerce e)) sz ar1 s2
          else
            let s3 = strictWriteAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# | e | #) #)
    (# ar1, s1, (# | | (# op, k #) #) #) ->
      let !(# s2, i #) = readAfArray @Int ar1 0# s1
          s3 = strictWriteAfArray ar1 0# (i - 1) s2
      in (# ar1, s3, (# | | (# op, \x -> doRunEscape' (k x) f g #) #) #)


{-# INLINE doRunEscape #-}
doRunEscape ::
  forall ref ex efs a b.
  Af (Escape ex ref : efs) a -> -- Effectful computation to execute.
  (a -> Af efs b) ->        -- Action used if computation returns
                            -- without taking the shortcut.
  (ex -> Af efs b) ->       -- Action used if shortcut is taken.
  Af efs b
doRunEscape af f g = inline (doRunEscape' af f g)


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
  let di = cellIndexEscapeDepth @(Escape ex ref) @efs sz
      s' = strictWriteAfArray @Int ar 0# (GHC.I# (escapeDepth di)) s
  in (# ar, s', (# | unsafeCoerce ex | #) #)


{-# INLINE takeEscape_ #-}
takeEscape_ :: forall ref ex efs. In (Escape ex ref) efs => ex -> Af efs ()
takeEscape_ = takeEscape @ref


{-# INLINE delimitEscape' #-}
delimitEscape' ::
  forall ref ex efs a b. In (Escape ex ref) efs =>
  Af efs a ->           -- Effectful computation to execute.
  (a -> Af efs b) ->    -- Action used on normal return.
  (ex -> Af efs b) ->   -- Action when the escape is taken.
  Af efs b
delimitEscape' af f g = Af $ \ sz ar0 s0 ->
  let di = cellIndexEscapeDepth @(Escape ex ref) @efs sz in
  case copyFromAfArray ar0 (cellIndex di) (fstI16Pair sz) s0 of
    (# s1, backup #) ->
      case unAf af sz ar0 s1 of
        (# ar1, s2, (# a | | #) #) ->
          unAf (f a) sz ar1 s2
        (# ar1, s2, (# | e | #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, d #) ->
              case unI# d GHC.==# escapeDepth di of
                1# ->
                  let s4 = copyToAfArray backup ar1 (cellIndex di) s3
                  in unAf (g (unsafeCoerce e)) sz ar1 s4
                _ ->
                  (# ar1, s3, (# | e | #) #)
        (# ar1, s2, (# | | (# op, k #) #) #) ->
          (# ar1, s2, (# | | (# op, \ x -> delimitEscape' @ref (k x) f g #) #) #)


{-# INLINE delimitEscape #-}
delimitEscape ::
  forall ref ex efs a b. In (Escape ex ref) efs =>
  Af efs a ->           -- Effectful computation to execute.
  (a -> Af efs b) ->    -- Action used on normal return.
  (ex -> Af efs b) ->   -- Action when the escape is taken.
  Af efs b
delimitEscape af f g = inline (delimitEscape' @ref af f g)


{-# INLINE catchEscape' #-}
catchEscape' ::
  forall ref ex efs a. In (Escape ex ref) efs =>
  Af efs a -> (ex -> Af efs a) -> Af efs a
catchEscape' af g = Af $ \ sz ar0 s0 ->
  let di = cellIndexEscapeDepth @(Escape ex ref) @efs sz in
  case copyFromAfArray ar0 (cellIndex di) (fstI16Pair sz) s0 of
    (# s1, backup #) ->
      case unAf af sz ar0 s1 of
        (# ar1, s2, (# a | | #) #) ->
          (# ar1, s2, (# a | | #) #)
        (# ar1, s2, (# | e | #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, d #) ->
              case unI# d GHC.==# escapeDepth di of
                1# ->
                  let s4 = copyToAfArray backup ar1 (cellIndex di) s3
                  in unAf (g (unsafeCoerce e)) sz ar1 s4
                _ ->
                  (# ar1, s3, (# | e | #) #)
        (# ar1, s2, (# | | (# op, k #) #) #) ->
          (# ar1, s2, (# | | (# op, \ x -> catchEscape' @ref (k x) g #) #) #)


-- catchEscape af g = delimitEscape af return g
{-# INLINE catchEscape #-}
catchEscape ::
  forall ref ex efs a. In (Escape ex ref) efs =>
  Af efs a -> (ex -> Af efs a) -> Af efs a
catchEscape af g = inline (catchEscape' @ref af g)
