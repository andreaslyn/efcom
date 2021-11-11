{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Efcom.Handle
  ( Handle
  , Handler
  , runHandle
  , delimitHandle
  , backtrackHandle
  ) where

import Control.Efcom.Internal.Effect
import Control.Efcom.Internal.ComArray
import Control.Efcom.Internal.I16Pair
import Control.Efcom.Internal.Com
import Control.Efcom.Internal.Util
import Control.Efcom.Internal.In

import Unsafe.Coerce (unsafeCoerce)

import GHC.Exts (inline, Any)
import qualified GHC.Exts as GHC


-- It is parhaps better to remove the dfs, and just use
--  ha x -> (x -> Com efs r) -> Com efs r
-- This requires corresponding change to the Com monad.
type Handler (ha :: [*] -> * -> *) (efs :: [*]) (r :: *) =
  forall dfs x. ha dfs x -> ComCont dfs x efs r -> Com efs r


doRunHandle' ::
  forall ref ha efs a r.
  (a -> r) -> Handler ha efs r -> Com (Handle ha ref : efs) a -> Com efs r
doRunHandle' ret h = \ ef -> Com $ \ s0 sz ar0 ->
  case unCom ef s0 (addSndI16Pair sz 1#) ar0 of
    (# ar1, s1, (# a | #) #) ->
      (# ar1, s1, (# ret a | #) #)
    (# ar1, s1, (# | (# e | #) #) #) ->
      let !(# s2, i #) = readComArray @Int ar1 0# s1
          s3 = strictWriteComArray ar1 0# (i - 1) s2
      in (# ar1, s3, (# | (# e | #) #) #)
    (# ar1, s1, (# | (# | k #) #) #) ->
      case readComArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 1
          then
            let !(# s3, op #) = readComArray @Any ar1 1# s2
                s4 = writeComArray ar1 1# undefinedElementComArray s3
            in unCom (h (unsafeCoerce op) (ComContScope (doRunHandle' ret h) k)) s4 sz ar1
          else
            let s3 = strictWriteComArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# | (# | ComContScope (doRunHandle' ret h) k #) #) #)


{-# INLINE doRunHandle #-}
doRunHandle ::
  forall ref ha efs a r.
  Com (Handle ha ref : efs) a -> (a -> r) -> Handler ha efs r -> Com efs r
doRunHandle ef ret h = inline (doRunHandle' ret h) ef


{-# INLINE runHandle #-}
runHandle ::
  forall ref ha efs a r.
  Com (Handle ha ref : efs) a -> (a -> r) -> Handler ha efs r -> Com efs r
runHandle ef ret h = Com $ \ s sz ar ->
  case isMaxI16PairValue (sndI16Pair sz) of
    1# -> error "exceeded maximal number of handle effects"
    _ -> unCom (doRunHandle ef ret h) s sz ar


delimitHandle' ::
  forall ref ha efs a r. In (Handle ha ref) efs =>
  (a -> r) -> Handler ha efs r -> Com efs a -> Com efs r
delimitHandle' ret h = \ ef -> Com $ \ s0 sz ar0 ->
  let di = cellIndexEscapeDepth @(Handle ha ref) @efs sz in
  case copyFromComArray ar0 (cellIndex di) (fstI16Pair sz) s0 of
    (# s1, backup #) ->
      case unCom ef s1 sz ar0 of
        (# ar1, s2, (# a | #) #) ->
          (# ar1, s2, (# ret a | #) #)
        (# ar1, s2, (# | (# e | #) #) #) ->
          (# ar1, s2, (# | (# e | #) #) #)
        (# ar1, s2, (# | (# | k #) #) #) ->
          case readComArray @Int ar1 0# s2 of
            (# s3, d #) ->
              case unI# d GHC.==# escapeDepth di of
                1# ->
                  let s4 = copyToComArray backup ar1 (cellIndex di) s3
                      !(# s5, op #) = readComArray @Any ar1 1# s4
                      s6 = writeComArray ar1 1# undefinedElementComArray s5
                  in unCom (h (unsafeCoerce op) (ComContScope (delimitHandle' @ref ret h) k)) s6 sz ar1
                _ ->
                  (# ar1, s3, (# | (# | ComContScope (delimitHandle' @ref ret h) k #) #) #)


{-# INLINE delimitHandle #-}
delimitHandle ::
  forall ref ha efs a r. In (Handle ha ref) efs =>
  Com efs a ->
  (a -> r) ->
  Handler ha efs r ->
  Com efs r
delimitHandle ef ret h = inline (delimitHandle' @ref ret h) ef


{-# INLINE backtrackHandle #-}
backtrackHandle :: 
  forall ref ha efs a. In (Handle ha ref) efs =>
  ha efs a -> Com efs a
backtrackHandle op = Com $ \ s0 sz ar ->
  let di = cellIndexEscapeDepth @(Handle ha ref) @efs sz
      s1 = strictWriteComArray @Int ar 0# (GHC.I# (escapeDepth di)) s0
      s2 = writeComArray @(ha efs a) ar 1# op s1
  in (# ar, s2, (# | (# | unsafeCoerce ComContNil #) #) #)
