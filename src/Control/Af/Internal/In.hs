{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Internal.In
  ( IsIn (..)
  , escapeDepthCellIndex
  , escapeDepth
  , cellIndex
  , In
  , AllIn
  ) where

import Control.Af.Internal.I16Pair
import Control.Af.Internal.Effect

import GHC.Exts (Int#, (-#))

import Data.Kind (Constraint)


class IsIn (e :: *) (efs :: [*]) where
  isInEscapeCellPair :: Int# -> I16Pair


{-# INLINE escapeDepthCellIndex #-}
escapeDepthCellIndex :: forall e efs. IsIn e efs => I16Pair -> I16Pair
escapeDepthCellIndex p = isInEscapeCellPair @e @efs (fstI16Pair p)


{-# INLINE cellIndex #-}
cellIndex :: I16Pair -> Int#
cellIndex = fstI16Pair


{-# INLINE escapeDepth #-}
escapeDepth :: I16Pair -> Int#
escapeDepth = sndI16Pair


instance IsIn (Cell ce ref) (Cell ce ref : efs) where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair sz = makeI16Pair (sz -# 1#) 0#


instance IsIn IOE (IOE : efs) where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair sz = makeI16Pair sz 0#


instance IsIn (STE st) (STE st : efs) where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair sz = makeI16Pair sz 0#


instance IsIn (Escape ex ref) (Escape ex ref : efs) where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair sz = makeI16Pair sz 1#


instance {-# OVERLAPPABLE #-} IsIn e (Effects d ++ efs) => IsIn e (d : efs)
  where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair = isInEscapeCellPair @e @(Effects d ++ efs)


instance {-# OVERLAPPABLE #-} IsIn e efs => IsIn e (Cell ce d : efs) where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair sz =
    minusToFstI16Pair (isInEscapeCellPair @e @efs sz) 1#


instance {-# OVERLAPPABLE #-} IsIn e efs => IsIn e (IOE : efs) where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair = isInEscapeCellPair @e @efs


instance {-# OVERLAPPABLE #-} IsIn e efs => IsIn e (STE st : efs) where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair = isInEscapeCellPair @e @efs


instance {-# OVERLAPPABLE #-} IsIn e efs => IsIn e (Escape ex ref : efs) where
  {-# INLINE isInEscapeCellPair #-}
  isInEscapeCellPair sz =
    addToSndI16Pair (isInEscapeCellPair @e @efs sz) 1#


type family In (e :: *) (efs :: [*]) where
  In IOE efs = IsIn IOE efs
  In (STE st) efs = IsIn (STE st) efs
  In (Cell ce i) efs = IsIn (Cell ce i) efs
  In (Escape ex i) efs = IsIn (Escape ex i) efs
  In e efs = AllIn (Effects e) efs


type family AllIn (ds :: [*]) (efs :: [*]) :: Constraint where
  AllIn '[] efs = ()
  AllIn '[d] efs = In d efs
  AllIn (d : ds) efs = (In d efs, AllIn ds efs)
