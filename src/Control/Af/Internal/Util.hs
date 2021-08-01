module Control.Af.Internal.Util
  ( unI#
  , unsafeCoerceState
  , unsafeCoerceAfArray
  ) where

import Control.Af.Internal.AfArray

import GHC.Exts (Int#, Int (..), State#, unsafeCoerce#)


{-# INLINE unI# #-}
unI# :: Int -> Int#
unI# (I# i) = i


{-# INLINE unsafeCoerceState #-}
unsafeCoerceState :: forall s t. State# s -> State# t
unsafeCoerceState = unsafeCoerce#


{-# INLINE unsafeCoerceAfArray #-}
unsafeCoerceAfArray :: forall s t. AfArray s -> AfArray t
unsafeCoerceAfArray = unsafeCoerce#
