{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

#include <MachDeps.h>

#if WORD_SIZE_IN_BITS < 32
#error "unexpected word size in bits < 32"
#endif

module Control.Af.Internal.In
  ( IsIn (..)
  , shortcutDepth
  , cellIndex
  , In
  , AllIn
  ) where

import Control.Af.Internal.Effect

import GHC.Exts
  ( Int#
  , (-#)
  , (+#)
  , (>#)
  , inline
  )
import qualified GHC.Exts as GHC

import Data.Kind (Constraint)


class IsIn (e :: *) (efs :: [*]) where
  shortcutDepthCellIndex :: Int# -> Int#


maxEffects :: () -> Int#
maxEffects _ = 32767#


cellIndexShift :: () -> Int#
cellIndexShift _ = 16#


shortcutDepthMask :: () -> Int#
shortcutDepthMask _ = 0xFF#


makeDepthIndex :: Int# -> Int# -> Int#
makeDepthIndex d i =
  case d ># inline maxEffects () of
    1# -> error "crossed maximal number of escape effects"
    _ ->
      case i ># inline maxEffects () of
        1# -> error "crossed maximal number of cell effects"
        _ -> GHC.orI# d (GHC.uncheckedIShiftL# i (inline cellIndexShift ()))


{-# INLINE shortcutDepth #-}
shortcutDepth :: Int# -> Int#
shortcutDepth i = GHC.andI# i (inline shortcutDepthMask ())


{-# INLINE cellIndex #-}
cellIndex :: Int# -> Int#
cellIndex i = GHC.uncheckedIShiftRL# i (inline cellIndexShift ())


instance IsIn (Cell ce ref) (Cell ce ref : efs) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz = inline makeDepthIndex 0# (sz -# 1#)


instance IsIn IOE (IOE : efs) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz = inline makeDepthIndex 0# sz


instance IsIn (STE st) (STE st : efs) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz = inline makeDepthIndex 0# sz


instance IsIn (Escape ex ref) (Escape ex ref : efs) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz = inline makeDepthIndex 1# sz


instance {-# OVERLAPPABLE #-} IsIn e (MeetEffect d efs) => IsIn e (d : efs)
  where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex = shortcutDepthCellIndex @e @(MeetEffect d efs)


instance {-# OVERLAPPABLE #-} IsIn e efs => IsIn e (Cell ce d : efs) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz =
    let i = shortcutDepthCellIndex @e @efs sz
    in inline makeDepthIndex (shortcutDepth i) (cellIndex i -# 1#)


instance {-# OVERLAPPABLE #-} IsIn e efs => IsIn e (IOE : efs) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex = shortcutDepthCellIndex @e @efs


instance {-# OVERLAPPABLE #-} IsIn e efs => IsIn e (STE st : efs) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex = shortcutDepthCellIndex @e @efs


instance {-# OVERLAPPABLE #-} IsIn e efs => IsIn e (Escape ex ref : efs) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz =
    let i = shortcutDepthCellIndex @e @efs sz
    in inline makeDepthIndex (shortcutDepth i +# 1#) (cellIndex i)


type family In (e :: *) (efs :: [*]) where
  In IOE efs = IsIn IOE efs
  In (STE st) efs = IsIn (STE st) efs
  In (Cell ce i) efs = IsIn (Cell ce i) efs
  In (Escape ex i) efs = IsIn (Escape ex i) efs
  In e efs = AllIn (ApplyEffect e) efs


type family AllIn (ds :: [*]) (efs :: [*]) :: Constraint where
  AllIn '[] efs = ()
  AllIn '[d] efs = In d efs
  AllIn (d : ds) efs = (In d efs, AllIn ds efs)
