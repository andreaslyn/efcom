module Control.Af.Internal where

import Control.Af.Af

import GHC.Exts (State#, unsafeCoerce#)


{-# INLINE unsafeCoerceState #-}
unsafeCoerceState :: forall s t. State# s -> State# t
unsafeCoerceState = unsafeCoerce#


{-# INLINE unsafeCoerceAfArray #-}
unsafeCoerceAfArray :: forall s t. AfArray s -> AfArray t
unsafeCoerceAfArray = unsafeCoerce#
