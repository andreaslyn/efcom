{-# LANGUAGE CPP #-}

#include <MachDeps.h>

#if WORD_SIZE_IN_BITS < 32
#error "unexpected word size in bits < 32"
#endif

module Control.Af.Internal.I16Pair
  ( I16Pair (..)
  , maxI16PairValue
  , isMaxI16PairValue
  , makeI16Pair
  , fstI16Pair
  , sndI16Pair
  , addToFstI16Pair
  , addToSndI16Pair
  , minusToFstI16Pair
  , minusToSndI16Pair
  ) where


import GHC.Exts
  ( Int#
  , (-#)
  , (+#)
  , (==#)
  , orI#
  , andI#
  , uncheckedIShiftL#
  , uncheckedIShiftRL#
  )


newtype I16Pair = I16Pair Int#


{-# INLINE maxI16PairValue #-}
maxI16PairValue :: () -> Int#
maxI16PairValue _ = 32767#


{-# INLINE isMaxI16PairValue #-}
isMaxI16PairValue :: Int# -> Int#
isMaxI16PairValue i = i ==# maxI16PairValue ()


{-# INLINE shiftValueI16Pair #-}
shiftValueI16Pair :: () -> Int#
shiftValueI16Pair _ = 16#


{-# INLINE maskValueI16Pair #-}
maskValueI16Pair :: () -> Int#
maskValueI16Pair _ = 0xFF#


{-# INLINE makeI16Pair #-}
makeI16Pair :: Int# -> Int# -> I16Pair
makeI16Pair l r =
  let r' = uncheckedIShiftL# r (shiftValueI16Pair ())
  in I16Pair (orI# l r')


{-# INLINE fstI16Pair #-}
fstI16Pair :: I16Pair -> Int#
fstI16Pair (I16Pair p) = andI# p (maskValueI16Pair ())


{-# INLINE sndI16Pair #-}
sndI16Pair :: I16Pair -> Int#
sndI16Pair (I16Pair p) = uncheckedIShiftRL# p (shiftValueI16Pair ())


{-# INLINE addToFstI16Pair #-}
addToFstI16Pair :: I16Pair -> Int# -> I16Pair
addToFstI16Pair (I16Pair p) i = I16Pair (p +# i)


{-# INLINE addToSndI16Pair #-}
addToSndI16Pair :: I16Pair -> Int# -> I16Pair
addToSndI16Pair (I16Pair p) i =
  I16Pair (p +# uncheckedIShiftL# i (shiftValueI16Pair ()))


{-# INLINE minusToFstI16Pair #-}
minusToFstI16Pair :: I16Pair -> Int# -> I16Pair
minusToFstI16Pair (I16Pair p) i = I16Pair (p -# i)


{-# INLINE minusToSndI16Pair #-}
minusToSndI16Pair :: I16Pair -> Int# -> I16Pair
minusToSndI16Pair (I16Pair p) i =
  I16Pair (p -# uncheckedIShiftL# i (shiftValueI16Pair ()))
