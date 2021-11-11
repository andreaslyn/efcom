{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Efcom.Cell
  ( Cell
  , runCell
  , delimitCell
  , localCell
  , readCell
  , writeCell
  , lazyWriteCell
  ) where

import Control.Efcom.Internal.Effect
import Control.Efcom.Internal.ComArray
import Control.Efcom.Internal.Util
import Control.Efcom.Internal.I16Pair
import Control.Efcom.Internal.Com
import Control.Efcom.Internal.In

import GHC.Exts (inline)
import qualified GHC.Exts as GHC


{-# INLINABLE doRunCell' #-}
doRunCell' ::
  forall ref ce efs a b.
  ce ->                       -- The initial cell state.
  (a -> ce -> Com efs b) -> -- Action called with result and final
                           -- cell state of the effect computation.
  Com (Cell ce ref : efs) a -> -- Effect computation to execute.
  Com efs b
doRunCell' ce k = \ ef -> Com $ \ s0 sz ar0 ->
  case appendComArray sz ar0 ce s0 of
    (# ar1, s1, sz' #) ->
      let !(# ar2, s2, r #) = unCom ef s1 sz' ar1
          i = fstI16Pair sz
          !(# s3, ce' #) = readComArray ar2 i s2
          s4 = writeComArray ar2 i undefinedElementComArray s3
      in case r of
          (# a | #) ->
            unCom (k a ce') s4 sz ar2
          (# | (# e | #) #) ->
            (# ar2, s4, (# | (# e | #) #) #)
          (# | (# | kf #) #) ->
            (# ar2, s4, (# | (# | ComContScope (doRunCell' ce' k) kf #) #) #)


{-# INLINE doRunCell #-}
doRunCell ::
  forall ref ce efs a b.
  Com (Cell ce ref : efs) a -> -- Effect computation to execute.
  ce ->                       -- The initial cell state.
  (a -> ce -> Com efs b) -> -- Action called with result and final
                           -- cell state of the effect computation.
  Com efs b
doRunCell ef ce k = inline doRunCell' ce k ef


{-# INLINE runCell #-}
runCell ::
  forall ref ce efs a b.
  Com (Cell ce ref : efs) a -> -- Effect computation to execute.
  ce ->                       -- The initial cell state.
  (a -> ce -> Com efs b) -> -- Action called with result and final
                           -- cell state of the effect computation.
  Com efs b
runCell ef ce k = Com $ \ s sz ar ->
  case isMaxI16PairValue (fstI16Pair sz) of
    1# -> error "exceeded maximal number of cell effects"
    _ -> unCom (doRunCell ef ce k) s sz ar


{-# INLINABLE delimitCell' #-}
delimitCell' ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  ce ->       -- The initial cell state.
  (a -> ce -> Com efs b) -> -- Success action, used when the scoped
                           -- computation finished without escape.
                           -- The first argument is the result of the
                           -- computation the second argument is the
                           -- final cell state. The cell state of Com efs
                           -- is restored to that before delimitCell.
  (ce -> ce -> ce) ->      -- On escape action. Used when an internal
                           -- escape is taken. The first argument is
                           -- the cell state before delimitCell. The
                           -- second argument is the cell state from
                           -- when the escape was taken. The result
                           -- is the new cell state after delimitCell.
                           -- Note that, as an optimisation, external
                           -- escapes do not cause this, only internal.
  Com efs a -> -- Effectful computation to execute.
  Com efs b
delimitCell' ce k g = \ ef -> Com $ \ s0 sz ar0 ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
      !(# s1, orig #) = readComArray ar0 (cellIndex di) s0
      s2 = writeComArray ar0 (cellIndex di) ce s1
      !(# ar1, s3, r #) = unCom ef s2 sz ar0
      !(# s4, ce' #) = readComArray ar1 (cellIndex di) s3 in
  case r of
    (# a | #) ->
      let s5 = writeComArray ar1 (cellIndex di) orig s4
      in unCom (k a ce') s5 sz ar1
    (# | d #) ->
     let !(# s5, c #) = readComArray @Int ar1 0# s4 in
     let s6 = case unI# c GHC.<=# escapeDepth di of
                1# -> -- Internal escape.
                  writeComArray ar1 (cellIndex di) (g orig ce') s5
                _ ->  -- External escape.
                  writeComArray ar1 (cellIndex di) orig s5 in 
      case d of
        (# e | #) ->
          (# ar1, s6, (# | (# e | #) #) #)
        (# | kf #) ->
          (# ar1, s6, (# | (# | ComContScope (delimitCell' @ref ce' k g) kf #) #) #)


{-# INLINE delimitCell #-}
delimitCell ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  Com efs a -> -- Effectful computation to execute.
  ce ->       -- The initial cell state.
  (a -> ce -> Com efs b) -> -- Success action, used when the scoped
                           -- computation finished without escape.
                           -- The first argument is the result of the
                           -- computation the second argument is the
                           -- final cell state. The cell state of Com efs
                           -- is restored to that before delimitCell.
  (ce -> ce -> ce) ->      -- On escape action. Used when an internal
                           -- escape is taken. The first argument is
                           -- the cell state before delimitCell. The
                           -- second argument is the cell state from
                           -- when the escape was taken. The result
                           -- is the new cell state after delimitCell.
                           -- Note that, as an optimisation, external
                           -- escapes do not cause this, only internal.
  Com efs b
delimitCell ef ce k g = inline (delimitCell' @ref ce k g) ef


{-# INLINABLE localCell' #-}
localCell' ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  ce ->       -- The initial cell state.
  (a -> ce -> Com efs b) -> -- Success action, used when the scoped
                           -- computation finished without escape.
                           -- The first argument is the result of the
                           -- computation the second argument is the
                           -- final cell state. The cell state of Com efs
                           -- is restored to that before localCell.
  Com efs a -> -- Effectful computation to execute.
  Com efs b
localCell' ce k = \ ef -> Com $ \ s0 sz ar0 ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
      !(# s1, orig #) = readComArray ar0 (cellIndex di) s0
      s2 = writeComArray ar0 (cellIndex di) ce s1
      !(# ar1, s3, r #) = unCom ef s2 sz ar0
      !(# s4, ce' #) = readComArray ar1 (cellIndex di) s3
      s5 = writeComArray ar1 (cellIndex di) orig s4 in
  case r of
    (# a | #) ->
      unCom (k a ce') s5 sz ar1
    (# | (# e | #) #) ->
      (# ar1, s5, (# | (# e | #) #) #)
    (# | (# | kf #) #) ->
      (# ar1, s5, (# | (# | ComContScope (localCell' @ref ce' k) kf #) #) #)


{-# INLINE localCell #-}
localCell ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  Com efs a -> -- Effectful computation to execute.
  ce ->       -- The initial cell state.
  (a -> ce -> Com efs b) -> -- Success action, used when the scoped
                           -- computation finished without escape.
                           -- The first argument is the result of the
                           -- computation the second argument is the
                           -- final cell state. The cell state of Com efs
                           -- is restored to that before localCell.
  Com efs b
localCell ef ce k = inline (localCell' @ref ce k) ef


{-# INLINE lazyWriteCell #-}
lazyWriteCell :: forall ref ce efs. In (Cell ce ref) efs => ce -> Com efs ()
lazyWriteCell ce = Com $ \ s sz ar ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
  in (# ar, writeComArray ar (cellIndex di) ce s, (# () | #) #)


{-# INLINE writeCell #-}
writeCell :: forall ref ce efs. In (Cell ce ref) efs => ce -> Com efs ()
writeCell ce = Com $ \ s sz ar ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
  in (# ar, strictWriteComArray ar (cellIndex di) ce s, (# () | #) #)


{-# INLINE readCell #-}
readCell :: forall ref ce efs. In (Cell ce ref) efs => Com efs ce
readCell = Com $ \ s sz ar ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
  in case readComArray ar (cellIndex di) s of
      (# s', ce #) -> (# ar, s', (# ce | #) #)
