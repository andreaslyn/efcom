module Old where

{-
{-# INLINE afArray #-}
afArray :: forall s. AfArray s -> STArray s Int Any
afArray (AfArray _ ar) = ar


{-# INLINE afRef #-}
afRef :: forall s. AfArray s -> STRef s Int
afRef (AfArray r _) = r


{-# INLINE getNumElementsAfArray #-}
getNumElementsAfArray :: forall s. AfArray s -> ST s Int
getNumElementsAfArray ar = readSTRef (afRef ar)


{-# INLINE setNumElementsAfArray #-}
setNumElementsAfArray :: forall s. AfArray s -> Int -> ST s ()
setNumElementsAfArray ar n = writeSTRef (afRef ar) n


{-# INLINE newAfArray #-}
newAfArray :: forall s. Int -> ST s (AfArray s)
newAfArray end = do
  r <- newSTRef 0
  ar <- Ar.unsafeNewArray_ (0, end)
  return (AfArray r ar)


{-# INLINE initialAfArray #-}
initialAfArray :: forall s. ST s (AfArray s)
initialAfArray = newAfArray 5


{-# INLINE invertIndexAfArray #-}
invertIndexAfArray :: forall s. AfArray s -> Int -> ST s Int
invertIndexAfArray ar i = do
  n <- getNumElementsAfArray ar
  return (n - i - 1)


{-# INLINE writeAfArray #-}
writeAfArray :: forall a s. AfArray s -> Int -> a -> ST s ()
writeAfArray ar i a =
  Ar.unsafeWrite (afArray ar) i (unsafeCoerce a)


{-# INLINE readAfArray #-}
readAfArray :: forall a s. AfArray s -> Int -> ST s a
readAfArray ar i =
  unsafeCoerce @(ST s Any) @(ST s a) (Ar.unsafeRead (afArray ar) i)


{-# INLINE copyAfArray #-}
copyAfArray :: forall s. Int -> AfArray s -> AfArray s -> ST s ()
copyAfArray nelems from to = aux 0
  where
    aux :: Int -> ST s ()
    aux i
      | i == nelems = return ()
      | otherwise = do
          a <- readAfArray from i
          writeAfArray to i a
          aux (i + 1)


{-# INLINE doubleAfArray #-}
doubleAfArray :: forall s. Int -> AfArray s -> ST s (AfArray s)
doubleAfArray nelems ar = do
  ar' <- newAfArray (nelems * 2)
  copyAfArray nelems ar ar'
  return ar'


{-# INLINE getCapacityAfArray #-}
getCapacityAfArray :: forall s. AfArray s -> ST s Int
getCapacityAfArray ar = Ar.getNumElements (afArray ar)


{-# INLINE pushAfArray #-}
pushAfArray :: forall a s. AfArray s -> a -> ST s (AfArray s)
pushAfArray ar a = do
  i <- getNumElementsAfArray ar
  setNumElementsAfArray ar (i + 1)
  n <- getCapacityAfArray ar
  if i < n
  then do
    writeAfArray ar i a
    return ar
  else do
    ar' <- doubleAfArray n ar
    writeAfArray ar' i a
    return ar'


{-# INLINE popAfArray #-}
popAfArray :: forall s. AfArray s -> ST s ()
popAfArray ar = do
  i <- getNumElementsAfArray ar
  let !i' = i - 1
  writeAfArray ar i' ()
  setNumElementsAfArray ar i'


{-# INLINE copyFrom #-}
copyFrom ::
  STArray s Int Any -> Int -> Int -> ST s [Any]
copyFrom from start las
  | start > las = return []
  | otherwise = do
      x <- Ar.unsafeRead from start
      xs <- copyFrom from (start + 1) las
      return (x : xs)


{-# INLINE copyTo #-}
copyTo ::
  STArray s Int Any -> Int -> [Any] -> ST s ()
copyTo _ _ [] = return ()
copyTo dest i (x : xs) = do
  Ar.unsafeWrite dest i x
  copyTo dest (i + 1) xs


{-# INLINE readFormIndexUp #-}
readFormIndexUp :: forall s. AfArray s -> Int -> ST s [Any]
readFormIndexUp ar i = do
  n <- getNumElementsAfArray ar
  let !newBound = n - i
  copyFrom (afArray ar) i newBound


{-# INLINE writeFromIndexUp #-}
writeFromIndexUp ::
  forall s. AfArray s -> [Any] -> Int -> ST s ()
writeFromIndexUp dest src i = copyTo (afArray dest) i src


{-# INLINE failST #-}
failST :: forall a e s. e -> ST s (Either Any a)
failST e = return (unsafeCoerce (Left e))


instance Functor (Af es) where
  {-# INLINE fmap #-}
  fmap f (Af k) = Af $ \ ar -> fmap (fmap f) (k ar)


instance Applicative (Af es) where
  {-# INLINE pure #-}
  pure a = Af $ \ _ -> return (Right a)

  {-# INLINE (<*>) #-}
  Af f <*> (Af x) = Af $ \ ar -> fmap (<*>) (f ar) <*> x ar


instance Monad (Af es) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  Af m >>= k = Af $ \ ar -> do
    r <- m ar
    case r of
      Left e -> return (Left e)
      Right a -> unAf (k a) ar


class Has (e :: Ef) (es :: [Ef]) where
  hasIndex :: Int


instance Has e (e : es) where
  {-# INLINE hasIndex #-}
  hasIndex = 0


instance {-# OVERLAPPABLE #-} Has e es => Has e (d : es) where
  {-# INLINE hasIndex #-}
  hasIndex = 1 + hasIndex @e @es


{-# INLINE evalAf #-}
evalAf :: forall a. Af '[] a -> a
evalAf m = runST evalToST
  where
    evalToST :: forall s. ST s a
    evalToST = do
      ar <- initialAfArray
      Right a <- unAf m ar
      return a


------------------- State ------------------------------------------


{-# INLINE runSt #-}
runSt ::
  forall e st es a b.
  Af ('StEf st e : es) a -> st -> (a -> st -> Af es b) -> Af es b
runSt m st k = Af $ \ ar -> do
  ar' <- pushAfArray ar st
  x <- unAf m ar'
  case x of
    Left e -> do
      popAfArray ar'
      return (Left e)
    Right a -> do
      i <- invertIndexAfArray ar 0
      st' <- readAfArray ar' i
      popAfArray ar'
      unAf (k a st') ar'


localSt ::
  Has ('StEf st e) es =>
  Af es a -> st -> (a -> st -> Af es b) -> Af es b
localSt = undefined


{-# INLINE putSt #-}
putSt :: forall e st es. Has ('StEf st e) es => st -> Af es ()
putSt st = Af $ \ ar -> do
  i <- invertIndexAfArray ar (hasIndex @('StEf st e) @es)
  writeAfArray ar i st
  return (Right ())


{-# INLINE getSt #-}
getSt :: forall e st es. Has ('StEf st e) es => Af es st
getSt = Af $ \ ar -> do
  i <- invertIndexAfArray ar (hasIndex @('StEf st e) @es)
  fmap Right (readAfArray ar i)

-------------------------- Exception -----------------------------


{-# INLINE runEx #-}
runEx ::
  forall e ex a b es.
  Af ('ExEf ex e : es) a -> (a -> Af es b) -> (ex -> Af es b) -> Af es b
runEx m f g = Af $ \ (ar :: AfArray s) -> do
  ar' <- pushAfArray @Int ar 0
  x <- unAf m ar'
  case x of
    Left e -> do
      i <- invertIndexAfArray ar 0
      t <- readAfArray @Int ar' i
      popAfArray ar'
      if t == 0
      then failST e
      else unAf (g (unsafeCoerce e)) ar'
    Right a -> do
      popAfArray ar'
      unAf (f a) ar'


{-# INLINE throwEx #-}
throwEx :: forall e ex a es. Has ('ExEf ex e) es => ex -> Af es a
throwEx ex = Af $ \ ar -> do
  i <- invertIndexAfArray ar (hasIndex @('ExEf ex e) @es)
  writeAfArray @Int ar i 1
  failST ex


{-# INLINE catchEx #-}
catchEx ::
  forall e ex a es b. Has ('ExEf ex e) es =>
  Af es a -> (a -> Af es b) -> (ex -> Af es b) -> Af es b
catchEx m f g = Af $ \ ar -> do
  i <- invertIndexAfArray ar (hasIndex @('ExEf ex e) @es)
  let !ii = i + 1
  backup <- readFormIndexUp ar ii
  x <- unAf m ar
  case x of
    Left e -> do
      t <- readAfArray @Int ar i
      if t == 0
      then
        failST e
      else do
        writeFromIndexUp ar backup ii
        writeAfArray @Int ar i 0
        unAf (g (unsafeCoerce e)) ar
    Right a ->
      unAf (f a) ar


------------------------------- Test ----------------------------

data TestState1 :: *
data TestState2 :: *
data TestExc1 :: *


testRun1 :: (Int, Int)
testRun1 =
  evalAf $ runSt @TestState1 test (0 :: Int) (\i s -> return (i, s))
  where
    test :: Af '[ 'StEf Int TestState1 ] Int
    test = do
      st <- getSt @TestState1
      putSt @TestState1 (st + 1)
      return st


testLoop ::
  Has ('StEf Bool TestState2) es =>
  Has ('StEf Int TestState1) es =>
  Has ('ExEf String TestExc1) es =>
  Int -> Af es Int
testLoop 0 = do
  x <- getSt @TestState1
  y <- getSt @TestState2
  if x < 0
  then throwEx @TestExc1 "fail!"
  else return $ if y then x + 1 else x
testLoop i =
  catchEx @TestExc1 @String (do
      x <- getSt @TestState1 @Int
      y <- getSt @TestState2 @Bool
      putSt @TestState1 (x + 1)
      putSt @TestState2 (not y)
      testLoop (i - 1)
    ) return (throwEx @TestExc1)


testLoopT ::
  (T.MonadError String m, T.MonadState Int m) =>
  Int -> T.StateT Bool m Int
testLoopT 0 = do
  x <- lift T.get
  y <- T.get
  if x < 0
  then T.throwError "fail!"
  else return $ if y then x + 1 else x
testLoopT i = flip T.catchError T.throwError $ do
  x <- lift T.get
  y <- T.get
  lift $ T.put (x + 1)
  T.put (not y)
  testLoopT (i - 1)


main :: IO ()
main = do
  --print (evalAf $ (runSt @TestState1 (runEx @TestExc1 @String (runSt @TestState2 (testLoop 1000000) False (\i s -> return (i :: Int, s :: Bool))) (return . Right) (return . Left)) (0 :: Int) (\i s -> return (i, s))))
  --print (evalAf $ runEx @TestExc1 @String (runSt @TestState1 (runSt @TestState2 (testLoop 1000000) False (\i s -> return (i :: Int, s :: Bool))) (0 :: Int) (\i s -> return (i, s))) (return . Right) (return . Left))
  --print (T.runExcept (T.runStateT (T.runStateT (testLoopT 1000000) False) 0))
-}
