{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Control.Af
import Control.Af.StateE
import Control.Af.ExceptE
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
  '[StateE Bool (Composite ())
  , StateE Int (OtherState (Composite ()))
  , ExceptE String ()] es =>
  String -> Af es (Int, Int)
catchClause _ = do
  !x <- get @(OtherState (Composite ()))
  !y <- get @(Composite ())
  return $ if y then (x + 1, x + 1) else (x, x)


data OtherState (i :: *)
type instance Effect OtherState = '[StateE Int]


unOtherState ::
  forall i es a.
  Af (OtherState i : es) a -> Af (MeetEffect OtherState i es) a
unOtherState = meetEffect


data Composite (i :: *)
type instance Effect Composite = '[StateE Bool, OtherState]


unComposite ::
  forall i es a.
  Af (Composite i : es) a -> Af (MeetEffect Composite i es) a
unComposite = meetEffect


{-# NOINLINE testLoop #-}
testLoop ::
  forall st es.
  AllIn '[Composite (), ExceptE String (), STE st] es =>
  STRef st Int -> Int -> Af es (Int, Int)
testLoop r 0 = do
  !x <- get @(OtherState (Composite ())) @Int
  !y <- get @(Composite ())
  !z <- liftST (readSTRef r)
  if x < 0
  then raise @() "fail!"
  else return $ if y then (z + 1, x + 1) else (z, x)
testLoop r i = do
  except @() @String (do
      !x <- get @(OtherState (Composite ())) @Int
      !y <- get @(Composite ()) @Bool
      !z <- liftST (readSTRef r)
      put @(OtherState (Composite ())) (x + 1)
      put @(Composite ()) (not y)
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
  forall st. Af '[ExceptE String (), STE st, IOE] (((Int, Int), Bool), Int)
loopWithST = do
  r <- liftST @st (newSTRef (0 :: Int))
  (runState @(OtherState (Composite ()))
          (unOtherState
            (runState @(Composite ())
              (liftIO (putStrLn "hello 2") >> unComposite (liftIO (putStrLn "hello 3") >> (testLoop r 1000000)))
            False (\i s -> return (i, s))))
        (0 :: Int) (\i s -> return (i, s)))


main :: IO ()
main = do
  x <- runAfSTIO $ do
    liftIO (putStrLn "hello 1")
    runExcept @() @String loopWithST
      (return . Right) (return . Left)
  print x
{-
  print $ runAf $
    (runExcept @() @String
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
