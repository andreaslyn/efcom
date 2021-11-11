{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Efcom.Escape
  ( Escape
  , runEscape
  , takeEscape
  , takeEscape_
  , delimitEscape
  , catchEscape
  ) where

import Control.Efcom.Internal.Effect
import Control.Efcom.Internal.ComArray
import Control.Efcom.Internal.Util
import Control.Efcom.Internal.I16Pair
import Control.Efcom.Internal.Com
import Control.Efcom.Internal.In

import Unsafe.Coerce (unsafeCoerce)

import GHC.Exts (inline)
import qualified GHC.Exts as GHC


{-# INLINE doRunEscape' #-}
doRunEscape' ::
  forall ref ex efs a b.
  (a -> Com efs b) ->        -- Action used if computation returns
                            -- without taking the shortcut.
  (ex -> Com efs b) ->       -- Action used if shortcut is taken.
  Com (Escape ex ref : efs) a -> -- Effectful computation to execute.
  Com efs b
doRunEscape' f g = \ ef -> Com $ \ s0 sz ar0 ->
  case unCom ef s0 (addSndI16Pair sz 1#) ar0 of
    (# ar1, s1, (# a | #) #) ->
      unCom (f a) s1 sz ar1
    (# ar1, s1, (# | (# e | #) #) #) ->
      case readComArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 1 then
            unCom (g (unsafeCoerce e)) s2 sz ar1
          else
            let s3 = strictWriteComArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# | (# e | #) #) #)
    (# ar1, s1, (# | (# | k #) #) #) ->
      let !(# s2, i #) = readComArray @Int ar1 0# s1
          s3 = strictWriteComArray ar1 0# (i - 1) s2
      in (# ar1, s3, (# | (# | ComContScope (doRunEscape' f g) k #) #) #)


{-# INLINE doRunEscape #-}
doRunEscape ::
  forall ref ex efs a b.
  Com (Escape ex ref : efs) a -> -- Effectful computation to execute.
  (a -> Com efs b) ->        -- Action used if computation returns
                            -- without taking the shortcut.
  (ex -> Com efs b) ->       -- Action used if shortcut is taken.
  Com efs b
doRunEscape ef f g = inline doRunEscape' f g ef


{-# INLINE runEscape #-}
runEscape ::
  forall ref ex efs a b.
  Com (Escape ex ref : efs) a -> -- Effectful computation to execute.
  (a -> Com efs b) ->        -- Action used if computation returns
                            -- without taking the shortcut.
  (ex -> Com efs b) ->       -- Action used if shortcut is taken.
  Com efs b
runEscape ef f g = Com $ \ s sz ar ->
  case isMaxI16PairValue (sndI16Pair sz) of
    1# -> error "exceeded maximal number of escape effects"
    _ -> unCom (doRunEscape ef f g) s sz ar


{-# INLINE takeEscape #-}
takeEscape :: forall ref ex efs a. In (Escape ex ref) efs => ex -> Com efs a
takeEscape ex = Com $ \ s sz ar ->
  let di = cellIndexEscapeDepth @(Escape ex ref) @efs sz
      s' = strictWriteComArray @Int ar 0# (GHC.I# (escapeDepth di)) s
  in (# ar, s', (# | (# unsafeCoerce ex | #) #) #)


{-# INLINE takeEscape_ #-}
takeEscape_ :: forall ref ex efs. In (Escape ex ref) efs => ex -> Com efs ()
takeEscape_ = takeEscape @ref


{-# INLINE delimitEscape' #-}
delimitEscape' ::
  forall ref ex efs a b. In (Escape ex ref) efs =>
  (a -> Com efs b) ->    -- Action used on normal return.
  (ex -> Com efs b) ->   -- Action when the escape is taken.
  Com efs a ->           -- Effectful computation to execute.
  Com efs b
delimitEscape' f g = \ ef -> Com $ \ s0 sz ar0 ->
  let di = cellIndexEscapeDepth @(Escape ex ref) @efs sz in
  case copyFromComArray ar0 (cellIndex di) (fstI16Pair sz) s0 of
    (# s1, backup #) ->
      case unCom ef s1 sz ar0 of
        (# ar1, s2, (# a | #) #) ->
          unCom (f a) s2 sz ar1
        (# ar1, s2, (# | (# e | #) #) #) ->
          case readComArray @Int ar1 0# s2 of
            (# s3, d #) ->
              case unI# d GHC.==# escapeDepth di of
                1# ->
                  let s4 = copyToComArray backup ar1 (cellIndex di) s3
                  in unCom (g (unsafeCoerce e)) s4 sz ar1
                _ ->
                  (# ar1, s3, (# | (# e | #) #) #)
        (# ar1, s2, (# | (# | k #) #) #) ->
          (# ar1, s2, (# | (# | ComContScope (delimitEscape' @ref f g) k #) #) #)


{-# INLINE delimitEscape #-}
delimitEscape ::
  forall ref ex efs a b. In (Escape ex ref) efs =>
  Com efs a ->           -- Effectful computation to execute.
  (a -> Com efs b) ->    -- Action used on normal return.
  (ex -> Com efs b) ->   -- Action when the escape is taken.
  Com efs b
delimitEscape ef f g = inline (delimitEscape' @ref f g) ef


{-# INLINE catchEscape' #-}
catchEscape' ::
  forall ref ex efs a. In (Escape ex ref) efs =>
  (ex -> Com efs a) -> Com efs a -> Com efs a
catchEscape' g = \ ef -> Com $ \ s0 sz ar0 ->
  let di = cellIndexEscapeDepth @(Escape ex ref) @efs sz in
  case copyFromComArray ar0 (cellIndex di) (fstI16Pair sz) s0 of
    (# s1, backup #) ->
      case unCom ef s1 sz ar0 of
        (# ar1, s2, (# a | #) #) ->
          (# ar1, s2, (# a | #) #)
        (# ar1, s2, (# | (# e | #) #) #) ->
          case readComArray @Int ar1 0# s2 of
            (# s3, d #) ->
              case unI# d GHC.==# escapeDepth di of
                1# ->
                  let s4 = copyToComArray backup ar1 (cellIndex di) s3
                  in unCom (g (unsafeCoerce e)) s4 sz ar1
                _ ->
                  (# ar1, s3, (# | (# e | #) #) #)
        (# ar1, s2, (# | (# | k #) #) #) ->
          (# ar1, s2, (# | (# | ComContScope (catchEscape' @ref g) k #) #) #)


-- catchEscape ef g = delimitEscape ef return g
{-# INLINE catchEscape #-}
catchEscape ::
  forall ref ex efs a. In (Escape ex ref) efs =>
  Com efs a -> (ex -> Com efs a) -> Com efs a
catchEscape ef g = inline (catchEscape' @ref g) ef
