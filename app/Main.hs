import Control.Af
import Control.Af.Cell
import Control.Af.Escape
import Control.Af.STE
import Control.Af.IOE
import Control.Af.State
import Control.Af.Handle

import Control.Exception (Exception, try, throwIO, AssertionFailed (..))

import Data.STRef (STRef, newSTRef, writeSTRef, readSTRef)


------------------------------- Test ---------------------------------


{-# NOINLINE catchClause #-}
catchClause ::
  AllIn
  '[Cell Bool Composite
  , Cell Int (OtherState Composite)
  , Escape String ()] es =>
  String -> Af es (Int, Int)
catchClause _ = do
  !x <- readCell @(OtherState Composite)
  !y <- readCell @Composite
  return $ if y then (x + 1, x + 1) else (x, x)


data OtherState (i :: *)
type instance Effect (OtherState i) = '[Cell Int]


unOtherState ::
  forall i es a.
  Af (OtherState i : es) a -> Af (Cell Int (OtherState i) : es) a
unOtherState = runAfHead


data Composite
type instance Effect Composite = '[Cell Bool, OtherState]


unComposite ::
  forall es a.
  Af (Composite : es) a -> Af (Cell Bool Composite : OtherState Composite : es) a
unComposite = runAfHead


{-# NOINLINE testLoop #-}
testLoop ::
  forall st es.
  AllIn '[Composite, Escape String (), STE st] es =>
  STRef st Int -> Int -> Af es (Int, Int)
testLoop r 0 = do
  x <- readCell @(OtherState Composite) @Int
  y <- readCell @Composite
  !z <- liftST (readSTRef r)
  if x < 0
  then takeEscape @() "fail!"
  else return $ if y then (z + 1, x + 1) else (z, x)
testLoop r i = do
  delimitEscape @() @String (do
      x <- readCell @(OtherState Composite) @Int
      y <- readCell @Composite @Bool
      !z <- liftST (readSTRef r)
      writeCell @(OtherState Composite) (x + 1)
      writeCell @Composite (not y)
      liftST (writeSTRef r (z + 1))
      testLoop r (i - 1)
    ) return catchClause


loopWithST ::
  forall st. Af '[Escape String (), STE st, IOE] (((Int, Int), Bool), Int)
loopWithST = do
  r <- liftST @st (newSTRef (0 :: Int))
  (runCell @(OtherState Composite)
          (unOtherState
            (runCell @Composite
              (liftIO (putStrLn "hello 2") >> unComposite (liftIO (putStrLn "hello 3") >> (testLoop r 1000000)))
            False (\i s -> return (i, s))))
        (0 :: Int) (\i s -> return (i, s)))


{-# NOINLINE countdownPut #-}
countdownPut :: In (State Int) efs => Af efs Int
countdownPut = do
  (>>=) (get @Int) (\ n ->
    if n < 0
    then pure n
    else
      (*>) (put (n - 1)) countdownPut
   )


{-# NOINLINE runCountdownPut #-}
runCountdownPut :: Int -> (Int, Int)
runCountdownPut n = runAfPure $ runState countdownPut n


data Nondet (efs :: [*]) (a :: *) =
  Empty | Choose (Af efs a) (Af efs a)


nondetHandler :: forall efs a. Handler [] Nondet efs a
nondetHandler Empty _ = return []
nondetHandler (Choose m1 m2) h = do
  n1 <- h m1
  n2 <- h m2
  return (n1 ++ n2)


mytry :: 
  forall e efs a.
  Exception e =>
  In IOE efs =>
  Af efs a -> Af efs (Either e a)
mytry =
  transIO $ \ un io -> do
    x <- try @e io
    case x of
      Right v -> return (fmap Right v)
      Left e -> return (fmap (\_ -> Left e) un)


testNondet ::
  In (Handle Nondet ()) efs =>
  In (State Int) efs =>
  In (State Bool) efs =>
  In IOE efs =>
  Af efs (Int, Int, Int)
testNondet = do
  b1 <- get @Bool
  x0 <- mytry @AssertionFailed (liftIO (putStrLn "hello") >> backtrackHandle @() (Choose (put @Bool (not b1) >> get @Int >> liftIO (throwIO (AssertionFailed "failed"))) (put @Int 1 >> fmap (1+) (get @Int))))
  x <- case x0 of
          Left _ -> return 100
          Right y -> return y
  b2 <- get @Bool
  y <- transaction @Int (get @Int >>= \ i -> put (i+1) >> backtrackHandle @() (Choose (put @Bool (not b2) >> get @Int) (backtrackHandle @() (Choose (backtrackHandle @() Empty) (get @Int >>= \ i -> put (i+1) >> get @Int))))) 2
  s <- get @Int
  return (x, y, s)


main :: IO ()
main = do
  x <- withIOE (runState @Int (runHandle @() (runState @Bool testNondet False) nondetHandler) 1)
  print x
  --print (runCountdownPut 1000000)
{-
  print $ pureAf $
    (runEscape @() @String
      (runCell @(OtherState Composite)
        (runCell @Composite
          (runCell @(OtherState Composite)
            (runCell @Composite
              (runCell @(OtherState Composite)
                (runCell @Composite
                  (runCell @(OtherState Composite)
                    (runCell @Composite
                      (runCell @(OtherState Composite)
                        (runCell @Composite
                          (runCell @(OtherState Composite)
                            (runCell @Composite
                              (runCell @(OtherState Composite)
                                (runCell @Composite
                                  (runCell @(OtherState Composite)
                                    (runCell @Composite
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
