{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Internal.In
  ( IsIn (..)
  , afExDepth
  , afStIndex
  , In
  , AllIn
  ) where

import Control.Af.Internal.Effect

import GHC.Exts (Int#, (-#))

import Data.Kind (Constraint)


class IsIn (e :: *) (es :: [*]) where
  afExDepthStIndex :: Int# -> (# Int, Int# #)


{-# INLINE afExDepth #-}
afExDepth :: forall e es. IsIn e es => Int
afExDepth = let !(# i, _ #) = afExDepthStIndex @e @es 0# in i


{-# INLINE afStIndex #-}
afStIndex :: forall e es. IsIn e es => Int# -> Int#
afStIndex sz = let !(# _, i #) = afExDepthStIndex @e @es sz in i


instance IsIn (StateE st e) (StateE st e : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    (# error "afExDepth of StateE is undefined", sz -# 1# #)


instance IsIn IOE (IOE : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    (# error "afExDepth of IOE is undefined", sz #)


instance IsIn (STE st) (STE st : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    (# error "afExDepth of STE is undefined", sz #)


instance IsIn (ExceptE ex e) (ExceptE ex e : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz = (# 0, sz #)


instance {-# OVERLAPPABLE #-}
         IsIn e (MeetEffect d es) =>
         IsIn e (d : es)
  where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex = afExDepthStIndex @e @(MeetEffect d es)


instance {-# OVERLAPPABLE #-} IsIn e es => IsIn e (StateE st d : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    let !(# e, s #) = afExDepthStIndex @e @es sz in (# e, s -# 1# #)


instance {-# OVERLAPPABLE #-} IsIn e es => IsIn e (IOE : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex = afExDepthStIndex @e @es


instance {-# OVERLAPPABLE #-} IsIn e es => IsIn e (STE st : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex = afExDepthStIndex @e @es


instance {-# OVERLAPPABLE #-} IsIn e es => IsIn e (ExceptE ex d : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    let !(# e, s #) = afExDepthStIndex @e @es sz in (# 1 + e, s #)


type family In (e :: *) (es :: [*]) where
  In IOE es = IsIn IOE es
  In (STE st) es = IsIn (STE st) es
  In (StateE st i) es = IsIn (StateE st i) es
  In (ExceptE ex i) es = IsIn (ExceptE ex i) es
  In e es = AllIn (ApplyEffect e) es


type family AllIn (ds :: [*]) (es :: [*]) :: Constraint where
  AllIn '[] es = ()
  AllIn '[d] es = In d es
  AllIn (d : ds) es = (In d es, AllIn ds es)
