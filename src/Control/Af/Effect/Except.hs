{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Effect.Except
  ( ExceptE
  , runExcept
  , raise
  , except
  ) where

import Control.Af.Effect
import Control.Af.Af
import Control.Af.In

import GHC.Exts
  ( Any
  , State#
  , Int#
  , (+#)
  , (<#)
  )

import Unsafe.Coerce (unsafeCoerce)


{-# INLINE runExcept #-}
runExcept ::
  forall e ex es a b.
  Af (ExceptE ex e : es) a -> (a -> Af es b) -> (ex -> Af es b) -> Af es b
runExcept af f g = Af $ \ sz ar0 s0 ->
  case unAf af sz ar0 s0 of
    (# ar1, s1, (# e | #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 0
          then
            unAf (g (unsafeCoerce e)) sz ar1 s2
          else
            let s3 = writeStrictAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# e | #) #)
    (# ar1, s1, (# | a #) #) ->
      unAf (f a) sz ar1 s1


{-# INLINE raise #-}
raise :: forall e ex es a. In (ExceptE ex e) es => ex -> Af es a
raise ex = Af $ \ _ ar s ->
  let s' = writeStrictAfArray @Int ar 0# (afExDepth @(ExceptE ex e) @es) s
  in (# ar, s', (# unsafeCoerce ex | #) #)



data BufList a =
  NilBuf | OneBuf a | TwoBuf a a | ConsBuf a a a (BufList a)


{-# INLINE copyFrom3 #-}
copyFrom3 ::
  forall s.
  Any -> Any -> Any -> AfArray s -> Int# -> Int# ->
  State# s -> (# State# s, BufList Any #)
copyFrom3 x1 x2 x3 from start cap s0 =
  case copyFrom from start cap s0 of
    (# s1, xs #) -> (# s1, ConsBuf x1 x2 x3 xs #)


{-# INLINE copyFrom2 #-}
copyFrom2 ::
  forall s.
  Any -> Any -> AfArray s -> Int# -> Int# ->
  State# s -> (# State# s, BufList Any #)
copyFrom2 x1 x2 from start cap s0 =
  case start <# cap of
    1# ->
      case readAfArray from start s0 of
        (# s1, x3 #) ->
          copyFrom3 x1 x2 x3 from (start +# 1#) cap s1
    _ -> (# s0, TwoBuf x1 x2 #)


{-# INLINE copyFrom1 #-}
copyFrom1 ::
  forall s.
  Any -> AfArray s -> Int# -> Int# ->
  State# s -> (# State# s, BufList Any #)
copyFrom1 x1 from start cap s0 =
  case start <# cap of
    1# ->
      case readAfArray from start s0 of
        (# s1, x2 #) ->
          copyFrom2 x1 x2 from (start +# 1#) cap s1
    _ -> (# s0, OneBuf x1 #)


copyFrom ::
  forall s.
  AfArray s -> Int# -> Int# -> State# s -> (# State# s, BufList Any #)
copyFrom from start cap s0 =
  case start <# cap of
    1# ->
      case readAfArray from start s0 of
        (# s1, x1 #) ->
          copyFrom1 x1 from (start +# 1#) cap s1
    _ -> (# s0, NilBuf #)



{-# INLINE readFormIndexUp #-}
readFormIndexUp ::
  forall s.
  AfArray s -> Int# -> Int# -> State# s -> (# State# s, BufList Any #)
readFormIndexUp ar i sz s = copyFrom ar i sz s


copyTo :: AfArray s -> Int# -> BufList Any -> State# s -> State# s
copyTo _ _ NilBuf s = s
copyTo dest i (OneBuf x1) s0 = writeAfArray dest i x1 s0
copyTo dest i (TwoBuf x1 x2) s0 =
  let s1 = writeAfArray dest i x1 s0
  in writeAfArray dest (i +# 1#) x2 s1
copyTo dest i (ConsBuf x1 x2 x3 xs) s0 =
  let s1 = writeAfArray dest i x1 s0
      s2 = writeAfArray dest (i +# 1#) x2 s1
      s3 = writeAfArray dest (i +# 2#) x3 s2
  in copyTo dest (i +# 3#) xs s3


{-# INLINE writeFromIndexUp #-}
writeFromIndexUp ::
  forall s. AfArray s -> BufList Any -> Int# -> State# s -> State# s
writeFromIndexUp dest src i s = copyTo dest i src s


{-# INLINE except #-}
except ::
  forall e ex es a b c. In (ExceptE ex e) es =>
  Af es a -> (a -> Af es b) -> (ex -> Af es b) -> Af es c -> Af es b
except af f g r = Af $ \ sz ar0 s0 ->
  let ix = afStIndex @(ExceptE ex e) @es sz in
  case readFormIndexUp ar0 ix sz s0 of
    (# s1, backup #) ->
      case unAf af sz ar0 s1 of
        (# ar1, s2, (# e | #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, i #) ->
              if i == afExDepth @(ExceptE ex e) @es
              then
                let s4 = writeFromIndexUp ar1 backup ix s3
                in unAf (g (unsafeCoerce e)) sz ar1 s4
              else
                let !(# ar2, s4, _ #) = unAf r sz ar1 s3
                in (# ar2, s4, (# e | #) #)
        (# ar1, s2, (# | a #) #) ->
          unAf (f a) sz ar1 s2
