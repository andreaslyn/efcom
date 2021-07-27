{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import GHC.Exts
  ( Any
  , State#
  , Int#
  )
import qualified GHC.Exts as GHC

import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad.Except as T
import qualified Control.Monad.State.Strict as T
import Control.Monad.Trans.Class (lift)


runI# :: Int -> Int#
runI# (GHC.I# i) = i


type AfArray (s :: *) = GHC.MutableArray# s Any


data Ef = StEf * * | ExEf * *


newtype Af (es :: [Ef]) (a :: *) =
  Af
  { runAf :: forall s.
      Int# -> AfArray s ->
      State# s -> (# AfArray s, State# s, (# Any | a #) #)
  }


instance Functor (Af es) where
  {-# INLINE fmap #-}
  fmap f af = Af $ \sz ar s ->
    case runAf af sz ar s of
      (# ar', s', (# e | #) #) -> (# ar', s', (# e | #) #)
      (# ar', s', (# | a #) #) -> (# ar', s', (# | f a #) #)


instance Applicative (Af es) where
  {-# INLINE pure #-}
  pure a = Af $ \ _ ar s -> (# ar, s, (# | a #) #)

  {-# INLINE (<*>) #-}
  ff <*> af = Af $ \ sz ar s ->
    case runAf ff sz ar s of
      (# ar1, s1, (# e | #) #) -> (# ar1, s1, (# e | #) #)
      (# ar1, s1, (# | f #) #) -> runAf (fmap f af) sz ar1 s1


instance Monad (Af es) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  mf >>= ff = Af $ \ sz ar s -> do
    case runAf mf sz ar s of
      (# ar', s', (# e | #) #) -> (# ar', s', (# e | #) #)
      (# ar', s', (# | a #) #) -> runAf (ff a) sz ar' s'


{-# NOINLINE undefinedAfElement #-}
undefinedAfElement :: Any
undefinedAfElement = error "undefined AfArray element"


{-# INLINE newAfArray #-}
newAfArray :: forall s. Int# -> State# s -> (# State# s, AfArray s #)
newAfArray n s = GHC.newArray# n undefinedAfElement s


{-# INLINE capacityAfArray #-}
capacityAfArray :: forall s. AfArray s -> Int#
capacityAfArray = GHC.sizeofMutableArray#


{-# INLINE initialAfArray #-}
initialAfArray :: forall s. State# s -> (# State# s, AfArray s #)
initialAfArray s = newAfArray 1# s -- TODO update this at some point


{-# INLINE writeAfArray #-}
writeAfArray ::
  forall a s. AfArray s -> Int# -> a -> State# s -> State# s
writeAfArray ar i a s = GHC.writeArray# ar i (unsafeCoerce a) s


{-# INLINE writeStrictAfArray #-}
writeStrictAfArray ::
  forall a s. AfArray s -> Int# -> a -> State# s -> State# s
writeStrictAfArray ar i !a s = GHC.writeArray# ar i (unsafeCoerce a) s


{-# INLINE readAfArray #-}
readAfArray ::
  forall a s. AfArray s -> Int# -> State# s -> (# State# s, a #)
readAfArray ar i s =
  case GHC.readArray# ar i s of
    (# s', a #) -> (# s', unsafeCoerce a #)


{-# INLINE doubleAfArray #-}
doubleAfArray ::
  forall s. AfArray s -> State# s -> (# State# s, AfArray s #)
doubleAfArray ar s =
  let cap = capacityAfArray ar
  in case newAfArray (cap GHC.*# 2#) s of
      (# s1, ar' #) ->
        let s2 = GHC.copyMutableArray# ar 0# ar' 0# cap s1
        in (# s2, ar' #)


{-# INLINE appendAfArray #-}
appendAfArray ::
  forall a s.
  Int# -> AfArray s -> a -> State# s -> (# AfArray s, State# s, Int# #)
appendAfArray sz ar a s = do
  case sz GHC.<# capacityAfArray ar of
    1# -> (# ar, writeAfArray ar sz a s, sz GHC.+# 1# #)
    _ ->
      case doubleAfArray ar s of
        (# s', ar' #) -> (# ar', writeAfArray ar' sz a s', sz GHC.+# 1# #)


-------------------------------- Generic -----------------------------


class Has (e :: Ef) (es :: [Ef]) where
  afExDepth :: Int
  afIndex :: Int# -> Int#


instance Has ('StEf st e) ('StEf st e : es) where
  {-# INLINE afExDepth #-}
  afExDepth = error "afExDepth of StEf is undefined"

  {-# INLINE afIndex #-}
  afIndex sz = sz GHC.-# 1#


instance Has ('ExEf ex e) ('ExEf ex e : es) where
  {-# INLINE afExDepth #-}
  afExDepth = 0

  {-# INLINE afIndex #-}
  afIndex sz = sz


instance {-# OVERLAPPABLE #-} Has e es => Has e ('StEf st d : es) where
  {-# INLINE afExDepth #-}
  afExDepth = afExDepth @e @es

  {-# INLINE afIndex #-}
  afIndex sz = afIndex @e @es sz GHC.-# 1#


instance {-# OVERLAPPABLE #-} Has e es => Has e ('ExEf ex d : es) where
  {-# INLINE afExDepth #-}
  afExDepth = 1 + afExDepth @e @es

  {-# INLINE afIndex #-}
  afIndex sz = afIndex @e @es sz


{-# INLINE evalAf #-}
evalAf :: forall a. Af '[] a -> a
evalAf af =
  case GHC.runRW# rw of
    (# _, a #) -> a
  where
    rw :: forall s. State# s -> (# State# s, a #)
    rw s0 =
      case initialAfArray s0 of
        (# s1, ar #) ->
          case runAf af 1# ar s1 of
            (# _, s2, (# _ | #) #) -> (# s2, error "TODO" #)
            (# _, s2, (# | a #) #) -> (# s2, a #)


------------------------------ State ---------------------------------


{-# INLINE runSt #-}
runSt ::
  forall e st es a b.
  Af ('StEf st e : es) a -> st -> (a -> st -> Af es b) -> Af es b
runSt af st k = Af $ \ sz ar0 s0 ->
  case appendAfArray sz ar0 st s0 of
    (# ar1, s1, sz' #) ->
      case runAf af sz' ar1 s1 of
        (# ar2, s2, (# e | #) #) ->
          (# ar2, writeAfArray ar2 sz undefinedAfElement s2, (# e | #) #)
        (# ar2, s2, (# | a #) #) ->
          case readAfArray ar2 sz s2 of
            (# s3, st' #) ->
              let s4 = writeAfArray ar2 sz undefinedAfElement s3
              in runAf (k a st') sz ar2 s4


{-# INLINE localSt #-}
localSt ::
  forall e st es a b.
  Has ('StEf st e) es =>
  Af es a -> st -> (a -> st -> Af es b) -> Af es b
localSt af st k = Af $ \ sz ar s0 ->
  let ix = afIndex @('StEf st e) @es sz in
  case readAfArray ar ix s0 of
    (# s1, orig #) ->
      let s2 = writeAfArray ar ix st s1 in
      case runAf af sz ar s2 of
        (# ar', s3, (# e | #) #) ->
          let s4 = writeAfArray ar' ix orig s3
          in (# ar', s4, (# e | #) #)
        (# ar', s3, (# | a #) #) ->
          case readAfArray ar' ix s3 of
            (# s4, st' #) ->
              let s5 = writeAfArray ar' ix orig s4
              in runAf (k a st') sz ar' s5


{-# INLINE putSt #-}
putSt :: forall e st es. Has ('StEf st e) es => st -> Af es ()
putSt st = Af $ \ sz ar s ->
  let i = afIndex @('StEf st e) @es sz
  in (# ar, writeAfArray ar i st s, (# | () #) #)


{-# INLINE getSt #-}
getSt :: forall e st es. Has ('StEf st e) es => Af es st
getSt = Af $ \ sz ar s ->
  let i = afIndex @('StEf st e) @es sz
  in case readAfArray ar i s of
      (# s', a #) -> (# ar, s', (# | a #) #)


------------------------------- Exception ----------------------------


{-# INLINE runEx #-}
runEx ::
  forall e ex a b es.
  Af ('ExEf ex e : es) a -> (a -> Af es b) -> (ex -> Af es b) -> Af es b
runEx af f g = Af $ \ sz ar0 s0 ->
  case runAf af sz ar0 s0 of
    (# ar1, s1, (# e | #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 0
          then
            runAf (g (unsafeCoerce e)) sz ar1 s2
          else
            let s3 = writeStrictAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# e | #) #)
    (# ar1, s1, (# | a #) #) ->
      runAf (f a) sz ar1 s1


{-# INLINE throwEx #-}
throwEx :: forall e ex a es. Has ('ExEf ex e) es => ex -> Af es a
throwEx ex = Af $ \ _ ar s ->
  let s' = writeStrictAfArray @Int ar 0# (afExDepth @('ExEf ex e) @es) s
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
  case start GHC.<# cap of
    1# ->
      case readAfArray from start s0 of
        (# s1, x3 #) ->
          copyFrom3 x1 x2 x3 from (start GHC.+# 1#) cap s1
    _ -> (# s0, TwoBuf x1 x2 #)


{-# INLINE copyFrom1 #-}
copyFrom1 ::
  forall s.
  Any -> AfArray s -> Int# -> Int# ->
  State# s -> (# State# s, BufList Any #)
copyFrom1 x1 from start cap s0 =
  case start GHC.<# cap of
    1# ->
      case readAfArray from start s0 of
        (# s1, x2 #) ->
          copyFrom2 x1 x2 from (start GHC.+# 1#) cap s1
    _ -> (# s0, OneBuf x1 #)


copyFrom ::
  forall s.
  AfArray s -> Int# -> Int# -> State# s -> (# State# s, BufList Any #)
copyFrom from start cap s0 =
  case start GHC.<# cap of
    1# ->
      case readAfArray from start s0 of
        (# s1, x1 #) ->
          copyFrom1 x1 from (start GHC.+# 1#) cap s1
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
  in writeAfArray dest (i GHC.+# 1#) x2 s1
copyTo dest i (ConsBuf x1 x2 x3 xs) s0 =
  let s1 = writeAfArray dest i x1 s0
      s2 = writeAfArray dest (i GHC.+# 1#) x2 s1
      s3 = writeAfArray dest (i GHC.+# 2#) x3 s2
  in copyTo dest (i GHC.+# 3#) xs s3


{-# INLINE writeFromIndexUp #-}
writeFromIndexUp ::
  forall s. AfArray s -> BufList Any -> Int# -> State# s -> State# s
writeFromIndexUp dest src i s = copyTo dest i src s


{-# INLINE catchEx #-}
catchEx ::
  forall e ex a es b. Has ('ExEf ex e) es =>
  Af es a -> (a -> Af es b) -> (ex -> Af es b) -> Af es b
catchEx af f g = Af $ \ sz ar0 s0 ->
  let ix = afIndex @('ExEf ex e) @es sz in
  case readFormIndexUp ar0 ix sz s0 of
    (# s1, backup #) ->
      case runAf af sz ar0 s1 of
        (# ar1, s2, (# e | #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, i #) ->
              if i == afExDepth @('ExEf ex e) @es
              then
                let s4 = writeFromIndexUp ar1 backup ix s3
                in runAf (g (unsafeCoerce e)) sz ar1 s4
              else
                (# ar1, s3, (# e | #) #)
        (# ar1, s2, (# | a #) #) ->
          runAf (f a) sz ar1 s2


------------------------------- Test --------------------------------


data TestState1 :: *
data TestState2 :: *
data TestExc1 :: *


{-# NOINLINE catchClause #-}
catchClause ::
  Has ('StEf Bool TestState2) es =>
  Has ('StEf Int TestState1) es =>
  Has ('ExEf String TestExc1) es =>
  String -> Af es Int
catchClause _ = do
  !x <- getSt @TestState1
  !y <- getSt @TestState2
  return $ if y then x + 1 else x


{-# NOINLINE testLoop #-}
testLoop ::
  Has ('StEf Bool TestState2) es =>
  Has ('StEf Int TestState1) es =>
  Has ('ExEf String TestExc1) es =>
  Int -> Af es Int
testLoop 0 = do
  !x <- getSt @TestState1
  !y <- getSt @TestState2
  if x >= 0
  then throwEx @TestExc1 "fail!"
  else return $ if y then x + 1 else x
testLoop i = do
  catchEx @TestExc1 @String (do
      !x <- getSt @TestState1 @Int
      !y <- getSt @TestState2 @Bool
      putSt @TestState1 (x + 1)
      putSt @TestState2 (not y)
      testLoop (i - 1)
    ) return catchClause


{-# NOINLINE catchClauseT #-}
catchClauseT :: String -> T.StateT Bool (T.StateT Int (T.Except String)) Int
catchClauseT _ = do
  !x <- lift T.get
  !y <- T.get
  return $ if y then x + 1 else x


{-# NOINLINE testLoopT #-}
testLoopT ::
  Int -> T.StateT Bool (T.StateT Int (T.Except String)) Int
testLoopT 0 = do
  !x <- lift T.get
  !y <- T.get
  if x >= 0
  then T.throwError "fail!"
  else return $ if y then x + 1 else x
testLoopT i = flip T.catchError catchClauseT $ do
  !x <- lift T.get
  !y <- T.get
  lift $ T.put (x + 1)
  T.put (not y)
  testLoopT (i - 1)


main :: IO ()
main = do
  print $ evalAf $
    (runEx @TestExc1 @String
      (runSt @TestState1
        (runSt @TestState2
          (testLoop 1000000)
        False (\i s -> return (i, s)))
      (0 :: Int) (\i s -> return (i, s))))
    (return . Right) (return . Left)
{-
  print $ evalAf $
    (runEx @TestExc1 @String
      (runSt @TestState1
        (runSt @TestState2
          (runSt @TestState1
            (runSt @TestState2
              (runSt @TestState1
                (runSt @TestState2
                  (runSt @TestState1
                    (runSt @TestState2
                      (runSt @TestState1
                        (runSt @TestState2
                          (runSt @TestState1
                            (runSt @TestState2
                              (runSt @TestState1
                                (runSt @TestState2
                                  (runSt @TestState1
                                    (runSt @TestState2
                                      (testLoop 1000000)
                                    False (\ i s -> return (i, s)))
                                  (0 :: Int) (\ i s -> return (i, s)))
                                False (\ i _ -> return i))
                              (0 :: Int) (\ i _ -> return i))
                            False (\ i _ -> return i))
                          (0 :: Int) (\ i _ -> return i))
                        False (\ i _ -> return i))
                      (0 :: Int) (\ i _ -> return i))
                    False (\ i _ -> return i))
                  (0 :: Int) (\ i _ -> return i))
                False (\ i _ -> return i))
              (0 :: Int) (\ i _ -> return i))
            False (\ i _ -> return i))
          (0 :: Int) (\ i _ -> return i))
        False (\i _ -> return i))
      (0 :: Int) (\i _ -> return i)))
    (return . Right) (return . Left)
-}
  --print (T.runExceptT (T.runStateT (T.runStateT (testLoopT 1000000) False) 0))
