module Main where

import Control.Af
import Control.Af.Cell
import Control.Af.Escape
import Control.Af.STE
import Control.Af.IOE

import qualified Control.Monad.Writer as T
import qualified Control.Monad.Except as T
import qualified Control.Monad.State.Strict as T
import Control.Monad.Trans.Class (lift)

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
  Af (OtherState i : es) a -> Af (MeetEffect (OtherState i) es) a
unOtherState = meetEffect


data Composite
type instance Effect Composite = '[Cell Bool, OtherState]


unComposite ::
  forall es a.
  Af (Composite : es) a -> Af (MeetEffect Composite es) a
unComposite = meetEffect


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
  scopeEscape @() @String (do
      x <- readCell @(OtherState Composite) @Int
      y <- readCell @Composite @Bool
      !z <- liftST (readSTRef r)
      writeCell @(OtherState Composite) (x + 1)
      writeCell @Composite (not y)
      liftST (writeSTRef r (z + 1))
      testLoop r (i - 1)
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


main :: IO ()
main = do
  x <- withIOERunSTE $ do
    liftIO (putStrLn "hello 1")
    runEscape @() @String loopWithST
      (return . Right) (return . Left)
  print x
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
  --print (T.runExceptT (T.runStateT (T.runStateT (testLoopT 1000000) False) 0))
  --print $ T.runWriter (T.runExceptT globalWriterListen)
