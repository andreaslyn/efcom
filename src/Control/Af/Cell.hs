{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Cell
  ( Cell
  , runCell
  , scopeCell
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

import qualified GHC.Exts as GHC


{-# INLINE doRunCell #-}
doRunCell ::
  forall ref ce efs a b.
  Af (Cell ce ref : efs) a -> -- Effect computation to execute.
  ce ->                       -- The initial cell state.
  (a -> ce -> Af efs b) -> -- Action called with result and final
                           -- cell state of the effect computation.
  Af efs b
doRunCell af ce k = Af $ \ sz ar0 s0 ->
  case appendAfArray sz ar0 ce s0 of
    (# ar1, s1, sz' #) ->
      case unAf af sz' ar1 s1 of
        (# ar2, s2, (# e | #) #) ->
          let i = fstI16Pair sz
              s3 = writeAfArray ar2 i undefinedElementAfArray s2
          in (# ar2, s3, (# e | #) #)
        (# ar2, s2, (# | a #) #) ->
          let i = fstI16Pair sz in
          case readAfArray ar2 i s2 of
            (# s3, ce' #) ->
              let s4 = writeAfArray ar2 i undefinedElementAfArray s3
              in unAf (k a ce') sz ar2 s4


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


{-# INLINE scopeCell #-}
scopeCell ::
  forall ref ce efs a b. In (Cell ce ref) efs =>
  Af efs a -> -- Effectful computation to execute in a scope
  ce ->       -- The cell state to use in this scope
  (a -> ce -> Af efs b) -> -- Success action, used when the scoped
                           -- computation finished without exception.
                           -- The first argument is the result of the
                           -- somputation the second argument is the
                           -- final cell state. The cell state is
                           -- restored to that before scopeCell.
  (ce -> Af efs ()) ->     -- Faiure action, used when the scoped
                           -- computation finished by taking a shortcut.
                           -- The argument is the cell state from when
                           -- the shortcut was issued. The cell state
                           -- is restored to that before scopeCell.
  Af efs b
scopeCell af ce k g = Af $ \ sz ar0 s0 ->
  let di = escapeDepthCellIndex @(Cell ce ref) @efs sz in
  case readAfArray ar0 (cellIndex di) s0 of
    (# s1, orig #) ->
      let s2 = writeAfArray ar0 (cellIndex di) ce s1 in
      case unAf af sz ar0 s2 of
        (# ar1, s3, (# e | #) #) ->
           let !(# s4, c #) = readAfArray @Int ar1 0# s3 in
           case unI# c GHC.<=# escapeDepth di of
            1# -> -- Internal escape.
              let !(# s5, ce' #) = readAfArray ar1 (cellIndex di) s4
                  s6 = writeAfArray ar1 (cellIndex di) orig s5
                  !(# ar2, s7, _ #) = unAf (g ce') sz ar1 s6
              in (# ar2, s7, (# e | #) #)
            _ ->  -- External escape.
              let s5 = writeAfArray ar1 (cellIndex di) orig s4
              in (# ar1, s5, (# e | #) #)
        (# ar1, s3, (# | a #) #) ->
          case readAfArray ar1 (cellIndex di) s3 of
            (# s4, ce' #) ->
              let s5 = writeAfArray ar1 (cellIndex di) orig s4
              in unAf (k a ce') sz ar1 s5


{-# INLINE lazyWriteCell #-}
lazyWriteCell :: forall ref ce efs. In (Cell ce ref) efs => ce -> Af efs ()
lazyWriteCell ce = Af $ \ sz ar s ->
  let di = escapeDepthCellIndex @(Cell ce ref) @efs sz
  in (# ar, writeAfArray ar (cellIndex di) ce s, (# | () #) #)


{-# INLINE writeCell #-}
writeCell :: forall ref ce efs. In (Cell ce ref) efs => ce -> Af efs ()
writeCell !ce = Af $ \ sz ar s ->
  let di = escapeDepthCellIndex @(Cell ce ref) @efs sz
  in (# ar, writeAfArray ar (cellIndex di) ce s, (# | () #) #)


{-# INLINE readCell #-}
readCell :: forall ref ce efs. In (Cell ce ref) efs => Af efs ce
readCell = Af $ \ sz ar s ->
  let di = escapeDepthCellIndex @(Cell ce ref) @efs sz
  in case readAfArray ar (cellIndex di) s of
      (# s', ce #) -> (# ar, s', (# | ce #) #)
