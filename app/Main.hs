{-# LANGUAGE AllowAmbiguousTypes #-}

-- Enable UndecidableInstances for In0 instance:
{-# LANGUAGE UndecidableInstances #-}

module Main where

import GHC.Exts
  ( Any
  , State#
  , Int#
  )
import qualified GHC.Exts as GHC
import qualified GHC.IO as GHC
import GHC.ST (ST (..), runST)

import Data.Kind (Constraint)

import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad.Writer as T
import qualified Control.Monad.Except as T
import qualified Control.Monad.State.Strict as T
import Control.Monad.Trans.Class (lift)


type AfArray (s :: *) = GHC.MutableArray# s Any


data IOEf

data STEf st

data StateEf st i

data ExEf ex i


type family Effect (e :: * -> *) :: [* -> *]


type ApEffect (e :: * -> *) (i :: *) = ApplyAll (Effect e) (e i)


type family ApplyAll (fs :: [* -> *]) (x :: *) :: [*] where
  ApplyAll '[] x = '[]
  ApplyAll (f : fs) x = f x : ApplyAll fs x


type family (xs :: [*]) ++ (ys :: [*]) :: [*] where
  '[] ++ ys = ys
  (x : xs) ++ ys = x : xs ++ ys


newtype Af (es :: [*]) (a :: *) =
  Af
  { unAf :: forall s.
      Int# -> AfArray s ->
      State# s -> (# AfArray s, State# s, (# Any | a #) #)
  }


instance Functor (Af es) where
  {-# INLINE fmap #-}
  fmap f af = Af $ \sz ar s ->
    case unAf af sz ar s of
      (# ar', s', (# e | #) #) -> (# ar', s', (# e | #) #)
      (# ar', s', (# | a #) #) -> (# ar', s', (# | f a #) #)


instance Applicative (Af es) where
  {-# INLINE pure #-}
  pure a = Af $ \ _ ar s -> (# ar, s, (# | a #) #)

  {-# INLINE (<*>) #-}
  ff <*> af = Af $ \ sz ar s ->
    case unAf ff sz ar s of
      (# ar1, s1, (# e | #) #) -> (# ar1, s1, (# e | #) #)
      (# ar1, s1, (# | f #) #) -> unAf (fmap f af) sz ar1 s1


instance Monad (Af es) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  mf >>= ff = Af $ \ sz ar s -> do
    case unAf mf sz ar s of
      (# ar', s', (# e | #) #) -> (# ar', s', (# e | #) #)
      (# ar', s', (# | a #) #) -> unAf (ff a) sz ar' s'


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
initialAfArray s = newAfArray 2# s


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
      newSize = GHC.uncheckedIShiftL# cap 1#
  in case newAfArray newSize s of
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


-------------------------------- In ----------------------------------


class In0 (e :: *) (es :: [*]) where
  afExDepthStIndex :: Int# -> (# Int, Int# #)


{-# INLINE afExDepth #-}
afExDepth :: forall e es. In0 e es => Int
afExDepth = let !(# i, _ #) = afExDepthStIndex @e @es 0# in i


{-# INLINE afStIndex #-}
afStIndex :: forall e es. In0 e es => Int# -> Int#
afStIndex sz = let !(# _, i #) = afExDepthStIndex @e @es sz in i


instance In0 (StateEf st e) (StateEf st e : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    (# error "afExDepth of StateEf is undefined", sz GHC.-# 1# #)


instance In0 IOEf (IOEf : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    (# error "afExDepth of IOEf is undefined", sz #)


instance In0 (STEf st) (STEf st : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    (# error "afExDepth of STEf is undefined", sz #)


instance In0 (ExEf ex e) (ExEf ex e : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz = (# 0, sz #)


instance {-# OVERLAPPABLE #-}
         In0 e (ApEffect d i ++ es) =>
         In0 e (d i : es)
  where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex = afExDepthStIndex @e @(ApEffect d i ++ es)


instance {-# OVERLAPPABLE #-} In0 e es => In0 e (StateEf st d : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    let !(# e, s #) = afExDepthStIndex @e @es sz in (# e, s GHC.-# 1# #)


instance {-# OVERLAPPABLE #-} In0 e es => In0 e (IOEf : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex = afExDepthStIndex @e @es


instance {-# OVERLAPPABLE #-} In0 e es => In0 e (STEf st : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex = afExDepthStIndex @e @es


instance {-# OVERLAPPABLE #-} In0 e es => In0 e (ExEf ex d : es) where
  {-# INLINE afExDepthStIndex #-}
  afExDepthStIndex sz =
    let !(# e, s #) = afExDepthStIndex @e @es sz in (# 1 + e, s #)


type family In (e :: *) (es :: [*]) where
  In IOEf es = In0 IOEf es
  In (STEf st) es = In0 (STEf st) es
  In (StateEf st i) es = In0 (StateEf st i) es
  In (ExEf ex i) es = In0 (ExEf ex i) es
  In (e i) es = AllIn (ApEffect e i) es


type family AllIn (ds :: [*]) (es :: [*]) :: Constraint where
  AllIn '[] es = ()
  AllIn '[d] es = In d es
  AllIn (d : ds) es = (In d es, AllIn ds es)


--------------------------- Run Af -----------------------------------


{-# INLINE runAf# #-}
runAf# :: forall es a s. Af es a -> State# s -> (# State# s, a #)
runAf# af s0 =
  case initialAfArray s0 of
    (# s1, ar #) ->
      case unAf af 1# ar s1 of
        (# _, s2, (# _ | #) #) ->
          (# s2, error "Af exception unexpectedly escaped" #)
        (# _, s2, (# | a #) #) ->
          (# s2, a #)


{-# INLINE runAf #-}
runAf :: forall a. Af '[] a -> a
runAf af = case GHC.runRW# (runAf# af) of (# _, a #) -> a


{-# INLINE runAfIO #-}
runAfIO :: forall a. Af '[IOEf] a -> IO a
runAfIO af = GHC.IO (runAf# af)


{-# INLINE runAfST #-}
runAfST :: forall st a. Af '[STEf st] a -> ST st a
runAfST af = ST (runAf# af)


{-# INLINE evalAfST #-}
evalAfST :: forall a. (forall st. Af '[STEf st] a) -> a
evalAfST af = runST (runAfST af)


{-# INLINE flattenAf #-}
flattenAf :: forall e i es a. Af (e i : es) a -> Af (ApEffect e i ++ es) a
flattenAf = unsafeCoerce


------------------------------ State ---------------------------------


{-# INLINE runState #-}
runState ::
  forall e st es a b.
  Af (StateEf st e : es) a -> st -> (a -> st -> Af es b) -> Af es b
runState af st k = Af $ \ sz ar0 s0 ->
  case appendAfArray sz ar0 st s0 of
    (# ar1, s1, sz' #) ->
      case unAf af sz' ar1 s1 of
        (# ar2, s2, (# e | #) #) ->
          (# ar2, writeAfArray ar2 sz undefinedAfElement s2, (# e | #) #)
        (# ar2, s2, (# | a #) #) ->
          case readAfArray ar2 sz s2 of
            (# s3, st' #) ->
              let s4 = writeAfArray ar2 sz undefinedAfElement s3
              in unAf (k a st') sz ar2 s4


{-# INLINE localState #-}
localState ::
  forall e st es a b c. In (StateEf st e) es =>
  Af es a -> st -> (a -> st -> Af es b) -> (st -> Af es c) -> Af es b
localState af st k g = Af $ \ sz ar0 s0 ->
  let ix = afStIndex @(StateEf st e) @es sz in
  case readAfArray ar0 ix s0 of
    (# s1, orig #) ->
      let s2 = writeAfArray ar0 ix st s1 in
      case unAf af sz ar0 s2 of
        (# ar1, s3, (# e | #) #) ->
          case readAfArray ar1 ix s3 of
            (# s4, st' #) ->
              let s5 = writeAfArray ar1 ix orig s4
                  !(# ar2, s6, _ #) = unAf (g st') sz ar1 s5
              in (# ar2, s6, (# e | #) #)
        (# ar1, s3, (# | a #) #) ->
          case readAfArray ar1 ix s3 of
            (# s4, st' #) ->
              let s5 = writeAfArray ar1 ix orig s4
              in unAf (k a st') sz ar1 s5


{-# INLINE putState #-}
putState :: forall e st es. In (StateEf st e) es => st -> Af es ()
putState st = Af $ \ sz ar s ->
  let i = afStIndex @(StateEf st e) @es sz
  in (# ar, writeAfArray ar i st s, (# | () #) #)


{-# INLINE getState #-}
getState :: forall e st es. In (StateEf st e) es => Af es st
getState = Af $ \ sz ar s ->
  let i = afStIndex @(StateEf st e) @es sz
  in case readAfArray ar i s of
      (# s', a #) -> (# ar, s', (# | a #) #)


------------------------------- Exception ----------------------------


{-# INLINE runEx #-}
runEx ::
  forall e ex es a b.
  Af (ExEf ex e : es) a -> (a -> Af es b) -> (ex -> Af es b) -> Af es b
runEx af f g = Af $ \ sz ar0 s0 ->
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


{-# INLINE throwEx #-}
throwEx :: forall e ex es a. In (ExEf ex e) es => ex -> Af es a
throwEx ex = Af $ \ _ ar s ->
  let s' = writeStrictAfArray @Int ar 0# (afExDepth @(ExEf ex e) @es) s
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
  forall e ex es a b c. In (ExEf ex e) es =>
  Af es a -> (a -> Af es b) -> (ex -> Af es b) -> Af es c -> Af es b
catchEx af f g r = Af $ \ sz ar0 s0 ->
  let ix = afStIndex @(ExEf ex e) @es sz in
  case readFormIndexUp ar0 ix sz s0 of
    (# s1, backup #) ->
      case unAf af sz ar0 s1 of
        (# ar1, s2, (# e | #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, i #) ->
              if i == afExDepth @(ExEf ex e) @es
              then
                let s4 = writeFromIndexUp ar1 backup ix s3
                in unAf (g (unsafeCoerce e)) sz ar1 s4
              else
                let !(# ar2, s4, _ #) = unAf r sz ar1 s3
                in (# ar2, s4, (# e | #) #)
        (# ar1, s2, (# | a #) #) ->
          unAf (f a) sz ar1 s2


-------------------------------- ST ----------------------------------


{-# INLINE unsafeCoerceState #-}
unsafeCoerceState :: forall s t. State# s -> State# t
unsafeCoerceState = GHC.unsafeCoerce#


{-# INLINE unsafeCoerceAfArray #-}
unsafeCoerceAfArray :: forall s t. AfArray s -> AfArray t
unsafeCoerceAfArray = GHC.unsafeCoerce#


data AfEnv s a =
    AfEnvError !(AfArray s) Any
  | AfEnvSuccess !(AfArray s) a


{-# INLINE unsafeAfEnvError #-}
unsafeAfEnvError :: forall s t a. AfArray s -> Any -> AfEnv t a
unsafeAfEnvError ar e = AfEnvError (unsafeCoerceAfArray ar) e


{-# INLINE unsafeAfEnvSuccess #-}
unsafeAfEnvSuccess :: forall s t a. AfArray s -> a -> AfEnv t a
unsafeAfEnvSuccess ar a = AfEnvSuccess (unsafeCoerceAfArray ar) a


{-# INLINE applyST #-}
applyST :: forall st a. ST st a -> State# st -> (# State# st, a #)
applyST (ST f) = f


type AfToST st es = forall a. Af es a -> ST st (AfEnv st a)


{-# INLINE unsafeAfToST #-}
unsafeAfToST :: forall s st es. Int# -> AfArray s -> AfToST st es
unsafeAfToST sz ar = \af -> ST $ \s ->
  case unAf af sz ar (unsafeCoerceState s) of
    (# ar', s', (# e | #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvError ar' e #)
    (# ar', s', (# | a #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvSuccess ar' a #)


{-# INLINE controlST #-}
controlST ::
  forall st es a. In (STEf st) es =>
  (AfToST st es -> ST st (AfEnv st a)) -> Af es a
controlST f = Af $ \ sz ar0 s0 ->
  case applyST (f (unsafeAfToST sz ar0)) (unsafeCoerceState s0) of
    (# s1, AfEnvError ar1 e #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# e | #) #)
    (# s1, AfEnvSuccess ar1 a #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# | a #) #)


{-# INLINE doST #-}
doST :: forall st es a. In (STEf st) es => ST st a -> Af es a
doST st = Af $ \ _ ar s0 ->
  let !(# s1, a #) = applyST st (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# | a #) #)


---------------------------------- IO --------------------------------


newtype AfEnvIO a = AfEnvIO (AfEnv GHC.RealWorld a)


{-# INLINE unsafeAfEnvIOError #-}
unsafeAfEnvIOError :: forall s a. AfArray s -> Any -> AfEnvIO a
unsafeAfEnvIOError ar e = AfEnvIO (unsafeAfEnvError ar e)


{-# INLINE unsafeAfEnvIoSuccess #-}
unsafeAfEnvIoSuccess :: forall s a. AfArray s -> a -> AfEnvIO a
unsafeAfEnvIoSuccess ar a = AfEnvIO (unsafeAfEnvSuccess ar a)


type AfToIO es = forall a. Af es a -> IO (AfEnvIO a)


{-# INLINE unsafeAfToIO #-}
unsafeAfToIO :: forall es s. Int# -> AfArray s -> AfToIO es
unsafeAfToIO sz ar = \af -> GHC.IO $ \s ->
  case unAf af sz ar (unsafeCoerceState s) of
    (# ar', s', (# e | #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvIOError ar' e #)
    (# ar', s', (# | a #) #) ->
      (# unsafeCoerceState s', unsafeAfEnvIoSuccess ar' a #)


{-# INLINE unsafeControlIO #-}
unsafeControlIO ::
  forall es a.
  (AfToIO es -> IO (AfEnvIO a)) -> Af es a
unsafeControlIO f = Af $ \ sz ar0 s0 ->
  case GHC.unIO (f (unsafeAfToIO sz ar0)) (unsafeCoerceState s0) of
    (# s1, AfEnvIO (AfEnvError ar1 e) #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# e | #) #)
    (# s1, AfEnvIO (AfEnvSuccess ar1 a) #) ->
      (# unsafeCoerceAfArray ar1, unsafeCoerceState s1, (# | a #) #)


{-# INLINE unsafeDoIO #-}
unsafeDoIO :: forall es a. IO a -> Af es a
unsafeDoIO io = Af $ \ _ ar s0 ->
  let !(# s1, a #) = GHC.unIO io (unsafeCoerceState s0)
  in (# ar, unsafeCoerceState s1, (# | a #) #)


{-# INLINE controlIO #-}
controlIO ::
  forall es a. In IOEf es =>
  (AfToIO es -> IO (AfEnvIO a)) -> Af es a
controlIO = unsafeControlIO


{-# INLINE doIO #-}
doIO :: forall es a. In IOEf es => IO a -> Af es a
doIO = unsafeDoIO


------------------------------- Test ---------------------------------


{-# NOINLINE catchClause #-}
catchClause ::
  AllIn
  '[StateEf Bool (Composite ())
  , StateEf Int (OtherState (Composite ()))
  , ExEf String ()] es =>
  String -> Af es Int
catchClause _ = do
  !x <- getState @(OtherState (Composite ()))
  !y <- getState @(Composite ())
  return $ if y then x + 1 else x


data OtherState (i :: *)
type instance Effect OtherState = '[StateEf Int]


unOtherState ::
  forall i es a.
  Af (OtherState i : es) a -> Af (ApEffect OtherState i ++ es) a
unOtherState = flattenAf


data Composite (i :: *)
type instance Effect Composite = '[StateEf Bool, OtherState]


unComposite ::
  forall i es a.
  Af (Composite i : es) a -> Af (ApEffect Composite i ++ es) a
unComposite = flattenAf


{-# NOINLINE testLoop #-}
testLoop ::
  AllIn '[Composite (), ExEf String ()] es =>
  Int -> Af es Int
testLoop 0 = do
  !x <- getState @(OtherState (Composite ())) @Int
  !y <- getState @(Composite ())
  if x < 0
  then throwEx @() "fail!"
  else return $ if y then x + 1 else x
testLoop i = do
  catchEx @() @String (do
      !x <- getState @(OtherState (Composite ())) @Int
      !y <- getState @(Composite ()) @Bool
      putState @(OtherState (Composite ())) (x + 1)
      putState @(Composite ()) (not y)
      testLoop (i - 1)
    ) return catchClause (return ())


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
  if x < 0
  then T.throwError "fail!"
  else return $ if y then x + 1 else x
testLoopT i = flip T.catchError catchClauseT $ do
  !x <- lift T.get
  !y <- T.get
  lift $ T.put (x + 1)
  T.put (not y)
  testLoopT (i - 1)


globalWriterListen :: T.ExceptT String (T.Writer String) ()
globalWriterListen = do
  T.pass (T.tell "hello" >> return ((), (\s -> "!"++s++"!")))


main :: IO ()
main = do
  x <- runAfIO $
    (runEx @() @String
      (runState @(OtherState (Composite ()))
        (unOtherState
          (runState @(Composite ())
            (unComposite (testLoop 1000000))
          False (\i s -> return (i, s))))
      (0 :: Int) (\i s -> return (i, s))))
    (return . Right) (return . Left)
  print x
{-
  print $ runAf $
    (runEx @() @String
      (runState @(OtherState (Composite ()))
        (runState @(Composite ())
          (runState @(OtherState (Composite ()))
            (runState @(Composite ())
              (runState @(OtherState (Composite ()))
                (runState @(Composite ())
                  (runState @(OtherState (Composite ()))
                    (runState @(Composite ())
                      (runState @(OtherState (Composite ()))
                        (runState @(Composite ())
                          (runState @(OtherState (Composite ()))
                            (runState @(Composite ())
                              (runState @(OtherState (Composite ()))
                                (runState @(Composite ())
                                  (runState @(OtherState (Composite ()))
                                    (runState @(Composite ())
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
  --print $ T.runWriter (T.runExceptT globalWriterListen)
