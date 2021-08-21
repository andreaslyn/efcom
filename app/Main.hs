{-# LANGUAGE AllowAmbiguousTypes #-}

import Control.Af
import Control.Af.Cell
import Control.Af.Escape
import Control.Af.STE
import Control.Af.IOE

import Data.STRef (STRef, newSTRef, writeSTRef, readSTRef)

import Data.Kind (Constraint)


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


data Nondet (c :: [*] -> Constraint) a

data NondetCont (c :: [*] -> Constraint) a = NondetCont (forall efs. (c efs, In (Nondet c a) efs) => Bool -> Af efs [a])

type instance Effect (Nondet c a) = '[Escape (NondetCont c a)]

nondetHandler ::
  forall c efs a. (c efs, In (Nondet c a) efs) =>
  Af efs [a] -> Af efs [a]
nondetHandler af = 
  catchEscape @(Nondet c a) @(NondetCont c a) af $ \ (NondetCont k) -> do
    xs <- nondetHandler @c (k True)
    ys <- nondetHandler @c (k False)
    return (xs ++ ys)

chooseBool ::
  forall c efs a. In (Nondet c a) efs =>
  (forall dfs. (c dfs, In (Nondet c a) dfs) => Bool -> Af dfs [a]) -> Af efs [a]
chooseBool k = takeEscape @(Nondet c a) (NondetCont k :: NondetCont c a)


class NoC (efs :: [*])

instance NoC efs


nondetTest :: forall efs. In (Nondet NoC Int) efs => (forall dfs. In (Nondet NoC Int) dfs => Int -> Af dfs [Int]) -> Af efs [Int]
nondetTest k = do
  chooseBool @NoC $ \ b ->
    if b
    then k 0
    else chooseBool @NoC $ \ c ->
      if c then k 1 else k 2


runNondet :: forall c efs a. c (Nondet c a : efs) => Af (Nondet c a : efs) [a] -> Af efs [a]
runNondet af =
  runEscape @(Nondet c a) @(NondetCont c a) (runAfHead (nondetHandler @c af)) return $ \ _ -> return []


main :: IO ()
main = do
  print (runAfPure (runNondet @NoC $ nondetTest @'[Nondet NoC Int] (return . return)))
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
