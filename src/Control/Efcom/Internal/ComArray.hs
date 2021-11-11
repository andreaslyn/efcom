module Control.Efcom.Internal.ComArray
  ( ComArray
  , unsafeCoerceComArray
  , undefinedElementComArray
  , newComArray
  , capacityComArray
  , writeComArray
  , strictWriteComArray
  , readComArray
  , doubleComArray
  , appendComArray
  , CopyBuf (..)
  , copyFromComArray
  , copyToComArray
  ) where

import Control.Efcom.Internal.I16Pair

import GHC.Exts
  ( Any
  , State#
  , Int#
  , (+#)
  , (<#)
  , unsafeCoerce#
  )
import qualified GHC.Exts as GHC


type ComArray (s :: *) = GHC.MutableArray# s Any


{-# INLINE unsafeCoerceComArray #-}
unsafeCoerceComArray :: forall s t. ComArray s -> ComArray t
unsafeCoerceComArray = unsafeCoerce#


{-# NOINLINE undefinedElementComArray #-}
undefinedElementComArray :: Any
undefinedElementComArray = error "undefined ComArray element"


{-# INLINE newComArray #-}
newComArray :: forall s. Int# -> State# s -> (# State# s, ComArray s #)
newComArray n s = GHC.newArray# n undefinedElementComArray s


{-# INLINE capacityComArray #-}
capacityComArray :: forall s. ComArray s -> Int#
capacityComArray = GHC.sizeofMutableArray#


{-# INLINE writeComArray #-}
writeComArray ::
  forall a s. ComArray s -> Int# -> a -> State# s -> State# s
writeComArray ar i a s = GHC.writeArray# ar i (unsafeCoerce# a) s


{-# INLINE strictWriteComArray #-}
strictWriteComArray ::
  forall a s. ComArray s -> Int# -> a -> State# s -> State# s
strictWriteComArray ar i !a s = GHC.writeArray# ar i (unsafeCoerce# a) s


{-# INLINE readComArray #-}
readComArray ::
  forall a s. ComArray s -> Int# -> State# s -> (# State# s, a #)
readComArray ar i s =
  case GHC.readArray# ar i s of
    (# s', a #) -> (# s', unsafeCoerce# a #)


{-# INLINE doubleComArray #-}
doubleComArray ::
  forall s. ComArray s -> State# s -> (# State# s, ComArray s #)
doubleComArray ar s =
  let cap = capacityComArray ar
      newSize = GHC.uncheckedIShiftL# cap 1#
  in case newComArray newSize s of
      (# s1, ar' #) ->
        let s2 = GHC.copyMutableArray# ar 0# ar' 0# cap s1
        in (# s2, ar' #)


{-# INLINE appendComArray #-}
appendComArray ::
  forall a s.
  I16Pair -> ComArray s -> a -> State# s -> (# ComArray s, State# s, I16Pair #)
appendComArray sz ar a s =
  let i = fstI16Pair sz in
  case i GHC.<# capacityComArray ar of
    1# ->
      (# ar, writeComArray ar i a s, addFstI16Pair sz 1# #)
    _ ->
      case doubleComArray ar s of
        (# s', ar' #) ->
          (# ar', writeComArray ar' i a s', addFstI16Pair sz 1# #)


----------------- Copy between ComArray and CopyBuf -------------------


data CopyBuf a =
  NilBuf | OneBuf a | TwoBuf a a | ConsBuf a a a (CopyBuf a)


{-# INLINE copyFrom3 #-}
copyFrom3 ::
  forall s.
  Any -> Any -> Any -> ComArray s -> Int# -> Int# ->
  State# s -> (# State# s, CopyBuf Any #)
copyFrom3 x1 x2 x3 ar i number s0 =
  case copyFromComArray ar i number s0 of
    (# s1, xs #) -> (# s1, ConsBuf x1 x2 x3 xs #)


{-# INLINE copyFrom2 #-}
copyFrom2 ::
  forall s.
  Any -> Any -> ComArray s -> Int# -> Int# ->
  State# s -> (# State# s, CopyBuf Any #)
copyFrom2 x1 x2 ar i number s0 =
  case i <# number of
    1# ->
      case readComArray ar i s0 of
        (# s1, x3 #) ->
          copyFrom3 x1 x2 x3 ar (i +# 1#) number s1
    _ -> (# s0, TwoBuf x1 x2 #)


{-# INLINE copyFrom1 #-}
copyFrom1 ::
  forall s.
  Any -> ComArray s -> Int# -> Int# ->
  State# s -> (# State# s, CopyBuf Any #)
copyFrom1 x1 ar i number s0 =
  case i <# number of
    1# ->
      case readComArray ar i s0 of
        (# s1, x2 #) ->
          copyFrom2 x1 x2 ar (i +# 1#) number s1
    _ -> (# s0, OneBuf x1 #)


copyFromComArray ::
  forall s.
  ComArray s -> Int# -> Int# -> State# s -> (# State# s, CopyBuf Any #)
copyFromComArray ar i number s0 =
  case i <# number of
    1# ->
      case readComArray ar i s0 of
        (# s1, x1 #) ->
          copyFrom1 x1 ar (i +# 1#) number s1
    _ -> (# s0, NilBuf #)


copyToComArray ::
  forall s. CopyBuf Any -> ComArray s -> Int# -> State# s -> State# s
copyToComArray NilBuf _ _ s = s
copyToComArray (OneBuf x1) dest i s0 = writeComArray dest i x1 s0
copyToComArray (TwoBuf x1 x2) dest i s0 =
  let s1 = writeComArray dest i x1 s0
  in writeComArray dest (i +# 1#) x2 s1
copyToComArray (ConsBuf x1 x2 x3 xs) dest i s0 =
  let s1 = writeComArray dest i x1 s0
      s2 = writeComArray dest (i +# 1#) x2 s1
      s3 = writeComArray dest (i +# 2#) x3 s2
  in copyToComArray xs dest (i +# 3#) s3
