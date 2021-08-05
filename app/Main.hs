import Control.Af
import Control.Af.Cell
import Control.Af.Escape
import Control.Af.STE
import Control.Af.IOE
import Control.Af.State

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


main :: IO ()
main = do
  print (runCountdownPut 1000000)
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
