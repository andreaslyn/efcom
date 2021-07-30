module Control.Af.Internal.Util
  ( unsafeCoerceState
  , unsafeCoerceAfArray
  ) where

import Control.Af.Internal.AfArray

import GHC.Exts (State#, unsafeCoerce#)


{-# INLINE unsafeCoerceState #-}
unsafeCoerceState :: forall s t. State# s -> State# t
unsafeCoerceState = unsafeCoerce#


{-# INLINE unsafeCoerceAfArray #-}
unsafeCoerceAfArray :: forall s t. AfArray s -> AfArray t
unsafeCoerceAfArray = unsafeCoerce#
