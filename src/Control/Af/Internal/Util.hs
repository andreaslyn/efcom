module Control.Af.Internal.Util
  ( unI#
  , unsafeCoerceState
  ) where

import GHC.Exts (Int#, Int (..), State#, unsafeCoerce#)


{-# INLINE unI# #-}
unI# :: Int -> Int#
unI# (I# i) = i


{-# INLINE unsafeCoerceState #-}
unsafeCoerceState :: forall s t. State# s -> State# t
unsafeCoerceState = unsafeCoerce#
