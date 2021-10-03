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

import GHC.Exts (inline, Any)
import qualified GHC.Exts as GHC


-- It is parhaps better to remove the dfs, and just use
--  ha x -> (x -> Af efs r) -> Af efs r
-- This requires corresponding change to the Af monad.
type Handler (ha :: [*] -> * -> *) (efs :: [*]) (r :: *) =
  forall dfs x. ha dfs x -> AfCont dfs x efs r -> Af efs r


doRunHandle' ::
  forall ref ha efs a r.
  (a -> r) -> Handler ha efs r -> Af (Handle ha ref : efs) a -> Af efs r
doRunHandle' ret h = \ af -> Af $ \ s0 sz ar0 ->
  case unAf af s0 (addSndI16Pair sz 1#) ar0 of
    (# ar1, s1, (# a | #) #) ->
      (# ar1, s1, (# ret a | #) #)
    (# ar1, s1, (# | (# e | #) #) #) ->
      let !(# s2, i #) = readAfArray @Int ar1 0# s1
          s3 = strictWriteAfArray ar1 0# (i - 1) s2
      in (# ar1, s3, (# | (# e | #) #) #)
    (# ar1, s1, (# | (# | k #) #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 1
          then
            let !(# s3, op #) = readAfArray @Any ar1 1# s2
                s4 = writeAfArray ar1 1# undefinedElementAfArray s3
            in unAf (h (unsafeCoerce op) (AfContScope (doRunHandle' ret h) k)) s4 sz ar1
          else
            let s3 = strictWriteAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# | (# | AfContScope (doRunHandle' ret h) k #) #) #)


{-# INLINE doRunHandle #-}
doRunHandle ::
  forall ref ha efs a r.
  Af (Handle ha ref : efs) a -> (a -> r) -> Handler ha efs r -> Af efs r
doRunHandle af ret h = inline (doRunHandle' ret h) af


{-# INLINE runHandle #-}
runHandle ::
  forall ref ha efs a r.
  Af (Handle ha ref : efs) a -> (a -> r) -> Handler ha efs r -> Af efs r
runHandle af ret h = Af $ \ s sz ar ->
  case isMaxI16PairValue (sndI16Pair sz) of
    1# -> error "exceeded maximal number of handle effects"
    _ -> unAf (doRunHandle af ret h) s sz ar


delimitHandle' ::
  forall ref ha efs a r. In (Handle ha ref) efs =>
  (a -> r) -> Handler ha efs r -> Af efs a -> Af efs r
delimitHandle' ret h = \ af -> Af $ \ s0 sz ar0 ->
  let di = cellIndexEscapeDepth @(Handle ha ref) @efs sz in
  case copyFromAfArray ar0 (cellIndex di) (fstI16Pair sz) s0 of
    (# s1, backup #) ->
      case unAf af s1 sz ar0 of
        (# ar1, s2, (# a | #) #) ->
          (# ar1, s2, (# ret a | #) #)
        (# ar1, s2, (# | (# e | #) #) #) ->
          (# ar1, s2, (# | (# e | #) #) #)
        (# ar1, s2, (# | (# | k #) #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, d #) ->
              case unI# d GHC.==# escapeDepth di of
                1# ->
                  let s4 = copyToAfArray backup ar1 (cellIndex di) s3
                      !(# s5, op #) = readAfArray @Any ar1 1# s4
                      s6 = writeAfArray ar1 1# undefinedElementAfArray s5
                  in unAf (h (unsafeCoerce op) (AfContScope (delimitHandle' @ref ret h) k)) s6 sz ar1
                _ ->
                  (# ar1, s3, (# | (# | AfContScope (delimitHandle' @ref ret h) k #) #) #)


{-# INLINE delimitHandle #-}
delimitHandle ::
  forall ref ha efs a r. In (Handle ha ref) efs =>
  Af efs a ->
  (a -> r) ->
  Handler ha efs r ->
  Af efs r
delimitHandle af ret h = inline (delimitHandle' @ref ret h) af


{-# INLINE backtrackHandle #-}
backtrackHandle :: 
  forall ref ha efs a. In (Handle ha ref) efs =>
  ha efs a -> Af efs a
backtrackHandle op = Af $ \ s0 sz ar ->
  let di = cellIndexEscapeDepth @(Handle ha ref) @efs sz
      s1 = strictWriteAfArray @Int ar 0# (GHC.I# (escapeDepth di)) s0
      s2 = writeAfArray @(ha efs a) ar 1# op s1
  in (# ar, s2, (# | (# | unsafeCoerce AfContNil #) #) #)
