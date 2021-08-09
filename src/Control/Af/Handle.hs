{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Handle
  ( Handle
  , Handler
  , runHandle
  , delimitHandle
  , backtrackHandle
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.I16Pair
import Control.Af.Internal.Af
import Control.Af.Internal.Util
import Control.Af.Internal.In

import Unsafe.Coerce (unsafeCoerce)

import GHC.Exts (inline)
import qualified GHC.Exts as GHC


-- It is parhaps better to remove the dfs, and just use
--  ha x -> (x -> Af efs r) -> Af efs r
-- This requires corresponding change to the Af monad.
type Handler (ha :: [*] -> * -> *) (efs :: [*]) (r :: *) =
  forall dfs x. ha dfs x -> (Af dfs x -> Af efs r) -> Af efs r


{-# INLINE doRunHandle' #-}
doRunHandle' ::
  forall ref ha efs a r.
  Af (Handle ha ref : efs) a -> (a -> r) -> Handler ha efs r -> Af efs r
doRunHandle' af ret h = Af $ \ sz ar0 s0 ->
  case unAf af (addSndI16Pair sz 1#) ar0 s0 of
    (# ar1, s1, (# a | | #) #) ->
      (# ar1, s1, (# ret a | | #) #)
    (# ar1, s1, (# | e | #) #) ->
      let !(# s2, i #) = readAfArray @Int ar1 0# s1
          s3 = strictWriteAfArray ar1 0# (i - 1) s2
      in (# ar1, s3, (# | e | #) #)
    (# ar1, s1, (# | | (# op, k #) #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 1
          then
            unAf (h (unsafeCoerce op) (\ x -> doRunHandle' (k x) ret h)) sz ar1 s2
          else
            let s3 = strictWriteAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# | | (# op, \x -> doRunHandle' (k x) ret h #) #) #)


{-# INLINE doRunHandle #-}
doRunHandle ::
  forall ref ha efs a r.
  Af (Handle ha ref : efs) a -> (a -> r) -> Handler ha efs r -> Af efs r
doRunHandle af ret h = inline (doRunHandle' af ret h)


{-# INLINE runHandle #-}
runHandle ::
  forall ref ha efs a r.
  Af (Handle ha ref : efs) a -> (a -> r) -> Handler ha efs r -> Af efs r
runHandle af ret h = Af $ \ sz ar s ->
  case isMaxI16PairValue (sndI16Pair sz) of
    1# -> error "exceeded maximal number of handle effects"
    _ -> unAf (doRunHandle af ret h) sz ar s


{-# INLINE delimitHandle' #-}
delimitHandle' ::
  forall ref ha efs a r. In (Handle ha ref) efs =>
  Af efs a -> (a -> r) -> Handler ha efs r -> Af efs r
delimitHandle' af ret h = Af $ \ sz ar0 s0 ->
  let di = cellIndexEscapeDepth @(Handle ha ref) @efs sz in
  case copyFromAfArray ar0 (cellIndex di) (fstI16Pair sz) s0 of
    (# s1, backup #) ->
      case unAf af sz ar0 s1 of
        (# ar1, s2, (# a | | #) #) ->
          (# ar1, s2, (# ret a | | #) #)
        (# ar1, s2, (# | e | #) #) ->
          (# ar1, s2, (# | e | #) #)
        (# ar1, s2, (# | | (# op, k #) #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, d #) ->
              case unI# d GHC.==# escapeDepth di of
                1# ->
                  let s4 = copyToAfArray backup ar1 (cellIndex di) s3
                  in unAf (h (unsafeCoerce op) (\ x -> delimitHandle' @ref (k x) ret h)) sz ar1 s4
                _ ->
                  (# ar1, s3, (# | | (# op, \ x -> delimitHandle' @ref (k x) ret h #) #) #)


{-# INLINE delimitHandle #-}
delimitHandle ::
  forall ref ha efs a r. In (Handle ha ref) efs =>
  Af efs a ->
  (a -> r) ->
  Handler ha efs r ->
  Af efs r
delimitHandle af ret h = inline (delimitHandle' @ref af ret h)



backtrackHandle :: 
  forall ref ha efs a. In (Handle ha ref) efs =>
  ha efs a -> Af efs a
backtrackHandle op = Af $ \ sz ar s ->
  let di = cellIndexEscapeDepth @(Handle ha ref) @efs sz
      s' = strictWriteAfArray @Int ar 0# (GHC.I# (escapeDepth di)) s
  in (# ar, s', (# | | (# unsafeCoerce op, unsafeCoerce id #) #) #)
