module Control.Af.Internal.AfArray
  ( AfArray
  , unsafeCoerceAfArray
  , undefinedElementAfArray
  , newAfArray
  , capacityAfArray
  , writeAfArray
  , writeStrictAfArray
  , readAfArray
  , doubleAfArray
  , appendAfArray
  , CopyBuf (..)
  , copyFromAfArray
  , copyToAfArray
  ) where

import Control.Af.Internal.I16Pair

import GHC.Exts
  ( Any
  , State#
  , Int#
  , (+#)
  , (<#)
  , unsafeCoerce#
  )
import qualified GHC.Exts as GHC


type AfArray (s :: *) = GHC.MutableArray# s Any


{-# INLINE unsafeCoerceAfArray #-}
unsafeCoerceAfArray :: forall s t. AfArray s -> AfArray t
unsafeCoerceAfArray = unsafeCoerce#


{-# NOINLINE undefinedElementAfArray #-}
undefinedElementAfArray :: Any
undefinedElementAfArray = error "undefined AfArray element"


{-# INLINE newAfArray #-}
newAfArray :: forall s. Int# -> State# s -> (# State# s, AfArray s #)
newAfArray n s = GHC.newArray# n undefinedElementAfArray s


{-# INLINE capacityAfArray #-}
capacityAfArray :: forall s. AfArray s -> Int#
capacityAfArray = GHC.sizeofMutableArray#


{-# INLINE writeAfArray #-}
writeAfArray ::
  forall a s. AfArray s -> Int# -> a -> State# s -> State# s
writeAfArray ar i a s = GHC.writeArray# ar i (unsafeCoerce# a) s


{-# INLINE writeStrictAfArray #-}
writeStrictAfArray ::
  forall a s. AfArray s -> Int# -> a -> State# s -> State# s
writeStrictAfArray ar i !a s = GHC.writeArray# ar i (unsafeCoerce# a) s


{-# INLINE readAfArray #-}
readAfArray ::
  forall a s. AfArray s -> Int# -> State# s -> (# State# s, a #)
readAfArray ar i s =
  case GHC.readArray# ar i s of
    (# s', a #) -> (# s', unsafeCoerce# a #)


{-# INLINE doubleAfArray #-}
doubleAfArray ::
  forall s. AfArray s -> State# s -> (# State# s, AfArray s #)
doubleAfArray ar s =
  let cap = capacityAfArray ar
      newSize = GHC.uncheckedIShiftL# cap 1#
  in case newAfArray newSize s of
      (# s1, ar' #) ->
        let s2 = GHC.copyMutableArray# ar 0# ar' 0# cap s1
        in (# s2, ar' #)


{-# INLINE appendAfArray #-}
appendAfArray ::
  forall a s.
  I16Pair -> AfArray s -> a -> State# s -> (# AfArray s, State# s, I16Pair #)
appendAfArray sz ar a s =
  let i = fstI16Pair sz in
  case i GHC.<# capacityAfArray ar of
    1# ->
      (# ar, writeAfArray ar i a s, addFstI16Pair sz 1# #)
    _ ->
      case doubleAfArray ar s of
        (# s', ar' #) ->
          (# ar', writeAfArray ar' i a s', addFstI16Pair sz 1# #)


----------------- Copy between AfArray and CopyBuf -------------------


data CopyBuf a =
  NilBuf | OneBuf a | TwoBuf a a | ConsBuf a a a (CopyBuf a)


{-# INLINE copyFrom3 #-}
copyFrom3 ::
  forall s.
  Any -> Any -> Any -> AfArray s -> Int# -> Int# ->
  State# s -> (# State# s, CopyBuf Any #)
copyFrom3 x1 x2 x3 ar i number s0 =
  case copyFromAfArray ar i number s0 of
    (# s1, xs #) -> (# s1, ConsBuf x1 x2 x3 xs #)


{-# INLINE copyFrom2 #-}
copyFrom2 ::
  forall s.
  Any -> Any -> AfArray s -> Int# -> Int# ->
  State# s -> (# State# s, CopyBuf Any #)
copyFrom2 x1 x2 ar i number s0 =
  case i <# number of
    1# ->
      case readAfArray ar i s0 of
        (# s1, x3 #) ->
          copyFrom3 x1 x2 x3 ar (i +# 1#) number s1
    _ -> (# s0, TwoBuf x1 x2 #)


{-# INLINE copyFrom1 #-}
copyFrom1 ::
  forall s.
  Any -> AfArray s -> Int# -> Int# ->
  State# s -> (# State# s, CopyBuf Any #)
copyFrom1 x1 ar i number s0 =
  case i <# number of
    1# ->
      case readAfArray ar i s0 of
        (# s1, x2 #) ->
          copyFrom2 x1 x2 ar (i +# 1#) number s1
    _ -> (# s0, OneBuf x1 #)


copyFromAfArray ::
  forall s.
  AfArray s -> Int# -> Int# -> State# s -> (# State# s, CopyBuf Any #)
copyFromAfArray ar i number s0 =
  case i <# number of
    1# ->
      case readAfArray ar i s0 of
        (# s1, x1 #) ->
          copyFrom1 x1 ar (i +# 1#) number s1
    _ -> (# s0, NilBuf #)


copyToAfArray ::
  forall s. CopyBuf Any -> AfArray s -> Int# -> State# s -> State# s
copyToAfArray NilBuf _ _ s = s
copyToAfArray (OneBuf x1) dest i s0 = writeAfArray dest i x1 s0
copyToAfArray (TwoBuf x1 x2) dest i s0 =
  let s1 = writeAfArray dest i x1 s0
  in writeAfArray dest (i +# 1#) x2 s1
copyToAfArray (ConsBuf x1 x2 x3 xs) dest i s0 =
  let s1 = writeAfArray dest i x1 s0
      s2 = writeAfArray dest (i +# 1#) x2 s1
      s3 = writeAfArray dest (i +# 2#) x3 s2
  in copyToAfArray xs dest (i +# 3#) s3
