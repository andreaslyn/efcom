{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Handle
  ( Handle
  , Handler
  , runHandle
  , backtrackHandle
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


type Handler (r :: * -> *) (ha :: [*] -> * -> *) (efs :: [*]) (a :: *) =
  forall dfs x. ha dfs x -> (Af dfs x -> Af efs (r a)) -> Af efs (r a)


{-# INLINE doRunHandle' #-}
doRunHandle' ::
  forall ref r ha efs a. Monad r =>
  Af (Handle ha ref : efs) a ->
  Handler r ha efs a ->
  Af efs (r a)
doRunHandle' af h = Af $ \ sz ar0 s0 ->
  case unAf af (addSndI16Pair sz 1#) ar0 s0 of
    (# ar1, s1, (# a | | #) #) ->
      (# ar1, s1, (# return a | | #) #)
    (# ar1, s1, (# | e | #) #) ->
      let !(# s2, i #) = readAfArray @Int ar1 0# s1
          s3 = strictWriteAfArray ar1 0# (i - 1) s2
      in (# ar1, s3, (# | e | #) #)
    (# ar1, s1, (# | | (# op, k #) #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 1
          then
            unAf (h (unsafeCoerce op) (\ x -> doRunHandle' (k x) h)) sz ar1 s2
          else
            let s3 = strictWriteAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# | | (# op, \x -> doRunHandle' (k x) h #) #) #)


{-# INLINE doRunHandle #-}
doRunHandle ::
  forall ref r ha efs a. Monad r =>
  Af (Handle ha ref : efs) a ->
  Handler r ha efs a ->
  Af efs (r a)
doRunHandle af h = inline (doRunHandle' af h)


{-# INLINE runHandle #-}
runHandle ::
  forall ref r ha efs a. Monad r =>
  Af (Handle ha ref : efs) a ->
  Handler r ha efs a ->
  Af efs (r a)
runHandle af h = Af $ \ sz ar s ->
  case isMaxI16PairValue (sndI16Pair sz) of
    1# -> error "exceeded maximal number of handle effects"
    _ -> unAf (doRunHandle af h) sz ar s


backtrackHandle :: 
  forall ref ha efs a. In (Handle ha ref) efs =>
  ha efs a -> Af efs a
backtrackHandle op = Af $ \ sz ar s ->
  let di = cellIndexEscapeDepth @(Handle ha ref) @efs sz
      s' = strictWriteAfArray @Int ar 0# (GHC.I# (escapeDepth di)) s
  in (# ar, s', (# | | (# unsafeCoerce op, unsafeCoerce id #) #) #)
