{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Internal.In
  ( IsIn (..)
  , shortcutDepth
  , cellIndex
  , In
  , AllIn
  ) where

import Control.Af.Internal.Effect

import GHC.Exts (Int#, (-#))

import Data.Kind (Constraint)


class IsIn (e :: *) (es :: [*]) where
  shortcutDepthCellIndex :: Int# -> (# Int, Int# #)


{-# INLINE shortcutDepth #-}
shortcutDepth :: forall e es. IsIn e es => Int
shortcutDepth = let !(# i, _ #) = shortcutDepthCellIndex @e @es 0# in i


{-# INLINE cellIndex #-}
cellIndex :: forall e es. IsIn e es => Int# -> Int#
cellIndex sz = let !(# _, i #) = shortcutDepthCellIndex @e @es sz in i


instance IsIn (Cell ce e) (Cell ce e : es) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz =
    (# error "shortcutDepth of Cell is undefined", sz -# 1# #)


instance IsIn IOE (IOE : es) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz =
    (# error "shortcutDepth of IOE is undefined", sz #)


instance IsIn (STE st) (STE st : es) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz =
    (# error "shortcutDepth of STE is undefined", sz #)


instance IsIn (Shortcut sh e) (Shortcut sh e : es) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz = (# 0, sz #)


instance {-# OVERLAPPABLE #-} IsIn e (MeetEffect d es) => IsIn e (d : es)
  where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex = shortcutDepthCellIndex @e @(MeetEffect d es)


instance {-# OVERLAPPABLE #-} IsIn e es => IsIn e (Cell ce d : es) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz =
    let !(# e, s #) = shortcutDepthCellIndex @e @es sz in (# e, s -# 1# #)


instance {-# OVERLAPPABLE #-} IsIn e es => IsIn e (IOE : es) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex = shortcutDepthCellIndex @e @es


instance {-# OVERLAPPABLE #-} IsIn e es => IsIn e (STE st : es) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex = shortcutDepthCellIndex @e @es


instance {-# OVERLAPPABLE #-} IsIn e es => IsIn e (Shortcut sh d : es) where
  {-# INLINE shortcutDepthCellIndex #-}
  shortcutDepthCellIndex sz =
    let !(# e, s #) = shortcutDepthCellIndex @e @es sz in (# 1 + e, s #)


type family In (e :: *) (es :: [*]) where
  In IOE es = IsIn IOE es
  In (STE st) es = IsIn (STE st) es
  In (Cell ce i) es = IsIn (Cell ce i) es
  In (Shortcut sh i) es = IsIn (Shortcut sh i) es
  In e es = AllIn (ApplyEffect e) es


type family AllIn (ds :: [*]) (es :: [*]) :: Constraint where
  AllIn '[] es = ()
  AllIn '[d] es = In d es
  AllIn (d : ds) es = (In d es, AllIn ds es)
