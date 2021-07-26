{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import GHC.Exts
  ( Any
  , State#
  , Int#
  )
import qualified GHC.Exts as GHC

import Unsafe.Coerce (unsafeCoerce)

--import qualified Control.Monad.Except as T
import qualified Control.Monad.State.Strict as T
import Control.Monad.Trans.Class (lift)


runInt# :: Int -> Int#
runInt# (GHC.I# i) = i


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


{-# INLINE flipAfIndex #-}
flipAfIndex :: Int# -> Int# -> Int#
flipAfIndex i sz = sz GHC.-# i GHC.-# 1#


{-# INLINE writeAfArray #-}
writeAfArray ::
  forall a s. AfArray s -> Int# -> a -> State# s -> State# s
writeAfArray ar i a s = GHC.writeArray# ar i (unsafeCoerce a) s


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
  hasIndex :: Int


instance Has ('StEf st e) ('StEf st e : es) where
  {-# INLINE hasIndex #-}
  hasIndex = 0


instance Has ('ExEf ex e) ('ExEf ex e : es) where
  {-# INLINE hasIndex #-}
  hasIndex = 0


instance {-# OVERLAPPABLE #-} Has e es => Has e ('StEf st d : es) where
  {-# INLINE hasIndex #-}
  hasIndex = 1 + hasIndex @e @es


instance {-# OVERLAPPABLE #-} Has e es => Has e ('ExEf ex d : es) where
  {-# INLINE hasIndex #-}
  hasIndex = 1 + hasIndex @e @es


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
          case runAf af 0# ar s1 of
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


localSt ::
  Has ('StEf st e) es =>
  Af es a -> st -> (a -> st -> Af es b) -> Af es b
localSt = undefined


{-# INLINE putSt #-}
putSt :: forall e st es. Has ('StEf st e) es => st -> Af es ()
putSt st = Af $ \ sz ar s ->
  let i = flipAfIndex (runInt# (hasIndex @('StEf st e) @es)) sz
  in (# ar, writeAfArray ar i st s, (# | () #) #)


{-# INLINE getSt #-}
getSt :: forall e st es. Has ('StEf st e) es => Af es st
getSt = Af $ \ sz ar s ->
  let i = flipAfIndex (runInt# (hasIndex @('StEf st e) @es)) sz
  in case readAfArray ar i s of
      (# s', a #) -> (# ar, s', (# | a #) #)


------------------------------- Test --------------------------------


data TestState1 :: *
data TestState2 :: *
data TestExc1 :: *


{-# NOINLINE testLoop #-}
testLoop ::
  Has ('StEf Bool TestState2) es =>
  Has ('StEf Int TestState1) es =>
  -- Has ('ExEf String TestExc1) es =>
  Int -> Af es Int
testLoop 0 = do
  !x <- getSt @TestState1
  !y <- getSt @TestState2
  if x < 0
  then return $ if y then x + 1 else x -- throwEx @TestExc1 "fail!"
  else return $ if y then x + 1 else x
testLoop i = do
  --catchEx @TestExc1 @String (do
      !x <- getSt @TestState1 @Int
      !y <- getSt @TestState2 @Bool
      putSt @TestState1 (x + 1)
      putSt @TestState2 (not y)
      testLoop (i - 1)
    --) return (throwEx @TestExc1)


{-# NOINLINE testLoopT #-}
testLoopT ::
  Int -> T.StateT Bool (T.State Int) Int
testLoopT 0 = do
  !x <- lift T.get
  !y <- T.get
  if x < 0
  then return $ if y then x + 1 else x -- T.throwError "fail!"
  else return $ if y then x + 1 else x
testLoopT i = do
  -- flip T.catchError T.throwError $ do
  !x <- lift T.get
  !y <- T.get
  lift $ T.put (x + 1)
  T.put (not y)
  testLoopT (i - 1)


main :: IO ()
main = do
  print (evalAf $
    (runSt @TestState1
      (runSt @TestState2
        (testLoop 1000000)
      False (\i s -> return (i :: Int, s :: Bool)))
    (0 :: Int) (\i s -> return (i, s))))
  --print (T.runState (T.runStateT (testLoopT 1000000) False) 0)
