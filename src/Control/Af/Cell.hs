{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Cell
  ( Cell
  , runCell
  , delimitCell
  , localCell
  , readCell
  , writeCell
  , lazyWriteCell
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Util
import Control.Af.Internal.I16Pair
import Control.Af.Internal.Af
import Control.Af.Internal.In

import GHC.Exts (inline)
import qualified GHC.Exts as GHC


{-# INLINABLE doRunCell' #-}
doRunCell' ::
  forall ref ce efs a b.
  Af (Cell ce ref : efs) a -> -- Effect computation to execute.
  ce ->                       -- The initial cell state.
  (a -> ce -> Af efs b) -> -- Action called with result and final
                           -- cell state of the effect computation.
  Af efs b
doRunCell' af ce k = Af $ \ sz ar0 s0 ->
  case appendAfArray sz ar0 ce s0 of
    (# ar1, s1, sz' #) ->
      let !(# ar2, s2, r #) = unAf af sz' ar1 s1
          i = fstI16Pair sz
          !(# s3, ce' #) = readAfArray ar2 i s2
          s4 = writeAfArray ar2 i undefinedElementAfArray s3
      in case r of
          (# a | #) ->
            unAf (k a ce') sz ar2 s4
          (# | (# e | #) #) ->
            (# ar2, s4, (# | (# e | #) #) #)
          (# | (# | kf #) #) ->
            (# ar2, s4, (# | (# | \x -> doRunCell' (kf x) ce' k #) #) #)


{-# INLINE doRunCell #-}
doRunCell ::
  forall ref ce efs a b.
  Af (Cell ce ref : efs) a -> -- Effect computation to execute.
  ce ->                       -- The initial cell state.
  (a -> ce -> Af efs b) -> -- Action called with result and final
                           -- cell state of the effect computation.
  Af efs b
doRunCell af ce k = inline (doRunCell' af ce k)


{-# INLINE runCell #-}
runCell ::
  forall ref ce efs a b.
  Af (Cell ce ref : efs) a -> -- Effect computation to execute.
  ce ->                       -- The initial cell state.
  (a -> ce -> Af efs b) -> -- Action called with result and final
                           -- cell state of the effect computation.
  Af efs b
runCell af ce k = Af $ \ sz ar s ->
  case isMaxI16PairValue (fstI16Pair sz) of
    1# -> error "exceeded maximal number of cell effects"
    _ -> unAf (doRunCell af ce k) sz ar s


{-# INLINABLE delimitCell' #-}
delimitCell' ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  Af efs a -> -- Effectful computation to execute.
  ce ->       -- The initial cell state.
  (a -> ce -> Af efs b) -> -- Success action, used when the scoped
                           -- computation finished without escape.
                           -- The first argument is the result of the
                           -- computation the second argument is the
                           -- final cell state. The cell state of Af efs
                           -- is restored to that before delimitCell.
  (ce -> ce -> ce) ->      -- On escape action. Used when an internal
                           -- escape is taken. The first argument is
                           -- the cell state before delimitCell. The
                           -- second argument is the cell state from
                           -- when the escape was taken. The result
                           -- is the new cell state after delimitCell.
                           -- Note that, as an optimisation, external
                           -- escapes do not cause this, only internal.
  Af efs b
delimitCell' af ce k g = Af $ \ sz ar0 s0 ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
      !(# s1, orig #) = readAfArray ar0 (cellIndex di) s0
      s2 = writeAfArray ar0 (cellIndex di) ce s1
      !(# ar1, s3, r #) = unAf af sz ar0 s2
      !(# s4, ce' #) = readAfArray ar1 (cellIndex di) s3 in
  case r of
    (# a | #) ->
      let s5 = writeAfArray ar1 (cellIndex di) orig s4
      in unAf (k a ce') sz ar1 s5
    (# | d #) ->
     let !(# s5, c #) = readAfArray @Int ar1 0# s4 in
     let s6 = case unI# c GHC.<=# escapeDepth di of
                1# -> -- Internal escape.
                  writeAfArray ar1 (cellIndex di) (g orig ce') s5
                _ ->  -- External escape.
                  writeAfArray ar1 (cellIndex di) orig s5 in 
      case d of
        (# e | #) ->
          (# ar1, s6, (# | (# e | #) #) #)
        (# | kf #) ->
          (# ar1, s6, (# | (# | \x -> delimitCell' @ref (kf x) ce' k g #) #) #)


{-# INLINE delimitCell #-}
delimitCell ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  Af efs a -> -- Effectful computation to execute.
  ce ->       -- The initial cell state.
  (a -> ce -> Af efs b) -> -- Success action, used when the scoped
                           -- computation finished without escape.
                           -- The first argument is the result of the
                           -- computation the second argument is the
                           -- final cell state. The cell state of Af efs
                           -- is restored to that before delimitCell.
  (ce -> ce -> ce) ->      -- On escape action. Used when an internal
                           -- escape is taken. The first argument is
                           -- the cell state before delimitCell. The
                           -- second argument is the cell state from
                           -- when the escape was taken. The result
                           -- is the new cell state after delimitCell.
                           -- Note that, as an optimisation, external
                           -- escapes do not cause this, only internal.
  Af efs b
delimitCell af ce k g = inline (delimitCell' @ref af ce k g)


{-# INLINABLE localCell' #-}
localCell' ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  Af efs a -> -- Effectful computation to execute.
  ce ->       -- The initial cell state.
  (a -> ce -> Af efs b) -> -- Success action, used when the scoped
                           -- computation finished without escape.
                           -- The first argument is the result of the
                           -- computation the second argument is the
                           -- final cell state. The cell state of Af efs
                           -- is restored to that before localCell.
  Af efs b
localCell' af ce k = Af $ \ sz ar0 s0 ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
      !(# s1, orig #) = readAfArray ar0 (cellIndex di) s0
      s2 = writeAfArray ar0 (cellIndex di) ce s1
      !(# ar1, s3, r #) = unAf af sz ar0 s2
      !(# s4, ce' #) = readAfArray ar1 (cellIndex di) s3
      s5 = writeAfArray ar1 (cellIndex di) orig s4 in
  case r of
    (# a | #) ->
      unAf (k a ce') sz ar1 s5
    (# | (# e | #) #) ->
      (# ar1, s5, (# | (# e | #) #) #)
    (# | (# | kf #) #) ->
      (# ar1, s5, (# | (# | \x -> localCell' @ref (kf x) ce' k #) #) #)


{-# INLINE localCell #-}
localCell ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  Af efs a -> -- Effectful computation to execute.
  ce ->       -- The initial cell state.
  (a -> ce -> Af efs b) -> -- Success action, used when the scoped
                           -- computation finished without escape.
                           -- The first argument is the result of the
                           -- computation the second argument is the
                           -- final cell state. The cell state of Af efs
                           -- is restored to that before localCell.
  Af efs b
localCell af ce k = inline (localCell' @ref af ce k)


{-# INLINE lazyWriteCell #-}
lazyWriteCell :: forall ref ce efs. In (Cell ce ref) efs => ce -> Af efs ()
lazyWriteCell ce = Af $ \ sz ar s ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
  in (# ar, writeAfArray ar (cellIndex di) ce s, (# () | #) #)


{-# INLINE writeCell #-}
writeCell :: forall ref ce efs. In (Cell ce ref) efs => ce -> Af efs ()
writeCell ce = Af $ \ sz ar s ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
  in (# ar, strictWriteAfArray ar (cellIndex di) ce s, (# () | #) #)


{-# INLINE readCell #-}
readCell :: forall ref ce efs. In (Cell ce ref) efs => Af efs ce
readCell = Af $ \ sz ar s ->
  let di = cellIndexEscapeDepth @(Cell ce ref) @efs sz
  in case readAfArray ar (cellIndex di) s of
      (# s', ce #) -> (# ar, s', (# ce | #) #)
