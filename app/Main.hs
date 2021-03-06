module Main where

import Control.Efcom
import Control.Efcom.Cell
import Control.Efcom.Escape
import Control.Efcom.STE
import Control.Efcom.IOE
import Control.Efcom.State
import Control.Efcom.Handle

import Control.Exception (Exception, try, throwIO, AssertionFailed (..))

import Data.STRef (STRef, newSTRef, writeSTRef, readSTRef)


------------------------------- Test ---------------------------------


{-# NOINLINE catchClause #-}
catchClause ::
  AllIn
  '[Cell Bool Composite
  , Cell Int (OtherState Composite)
  , Escape String ()] es =>
  String -> Com es (Int, Int)
catchClause _ = do
  !x <- readCell @(OtherState Composite)
  !y <- readCell @Composite
  return $ if y then (x + 1, x + 1) else (x, x)


data OtherState (i :: *)
type instance Effect (OtherState i) = '[Cell Int]


unOtherState ::
  forall i es a.
  Com (OtherState i : es) a -> Com (Cell Int (OtherState i) : es) a
unOtherState = runComHead


data Composite
type instance Effect Composite = '[Cell Bool, OtherState]


unComposite ::
  forall es a.
  Com (Composite : es) a -> Com (Cell Bool Composite : OtherState Composite : es) a
unComposite = runComHead


{-# NOINLINE testLoop #-}
testLoop ::
  forall st es.
  AllIn '[Composite, Escape String (), STE st] es =>
  STRef st Int -> Int -> Com es (Int, Int)
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
  forall st. Com '[Escape String (), STE st, IOE] (((Int, Int), Bool), Int)
loopWithST = do
  r <- liftST @st (newSTRef (0 :: Int))
  (runCell @(OtherState Composite)
          (unOtherState
            (runCell @Composite
              (liftIO (putStrLn "hello 2") >> unComposite (liftIO (putStrLn "hello 3") >> (testLoop r 1000000)))
            False (\i s -> return (i, s))))
        (0 :: Int) (\i s -> return (i, s)))


data Nondet (efs :: [*]) (a :: *) =
  Empty | Choose (Com efs a) (Com efs a)


nondetHandler :: forall efs a. Handler Nondet efs [a]
nondetHandler Empty _ = return []
nondetHandler (Choose m1 m2) h = do
  n1 <- runComCont1 h m1
  n2 <- runComCont1 h m2
  return (n1 ++ n2)


mytry :: 
  forall e efs a.
  Exception e =>
  In IOE efs =>
  Com efs a -> Com efs (Either e a)
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
  Com efs (Int, Int, Int)
testNondet = do
  b1 <- get @Bool
  x0 <- mytry @AssertionFailed (liftIO (putStrLn "hello") >> backtrackHandle @() (Choose (put @Bool (not b1) >> get @Int >> liftIO (throwIO (AssertionFailed "failed"))) (put @Int 1 >> fmap (1+) (get @Int))))
  x <- case x0 of
          Left _ -> return 100
          Right y -> return y
  b2 <- get @Bool
  y <- transaction @Int (get @Int >>= \ i -> put (i+1) >> backtrackHandle @() (Choose (put @Bool (not b2) >> get @Int) (backtrackHandle @() (Choose (backtrackHandle @() Empty) (get @Int >>= \ j -> put (j+1) >> get @Int))))) 2
  s <- get @Int
  return (x, y, s)


data ContFail :: [*] -> * -> * where
  ContFail :: forall (efs :: [*]) (a :: *). ContFail efs a


contFailHandler :: forall efs a. Handler ContFail efs (Maybe a)
contFailHandler ContFail _ = return Nothing


contStateCatchInternalError ::
  forall efs.
  AllIn '[Cell Int (), Handle ContFail ()] efs =>
  Com efs Int
contStateCatchInternalError = do
  writeCell @() (1 :: Int)
  m <- delimitHandle @() (do
          writeCell @() (2 :: Int)
          backtrackHandle @() ContFail
        ) Just contFailHandler
  case m of
    Nothing -> readCell @()
    Just _ -> error "what?"


main :: IO ()
main = do
  print $ runComPure $
    runCell @() @Int
          (runHandle @() @ContFail contStateCatchInternalError
            Just contFailHandler)
       0 (\ a s -> return (a, s))
  print $ runComPure $
    runHandle @() @ContFail
          (runCell @() @Int contStateCatchInternalError
              0 (\ a s -> return (a, s)))
       Just contFailHandler
{-
  let _ = loopWithST @Int
  x <- withIOE (runState @Int (runHandle @() (runState @Bool testNondet False) return nondetHandler) 1)
  print x
-}
{-
  print $ pureCom $
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
