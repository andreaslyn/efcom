module CellTest (cellTest) where

import Control.Af
import Control.Af.Cell
import Control.Af.Escape


data CellRef1
type Cell1 = Cell Int CellRef1

data CellRef2
type Cell2 = Cell Int CellRef2

data CellRef3
type Cell3 = Cell Int CellRef3

data CellRef4
type Cell4 = Cell Int CellRef4

data CellRef5
type Cell5 = Cell Int CellRef5

data EscapeRef1
type Escape1 = Escape Int EscapeRef1

data EscapeRef2
data EscapeRef3


runIntCell ::
  forall ref efs a. Int -> Af (Cell Int ref : efs) a -> Af efs (a, Int)
runIntCell ce af =
  runCell @ref @Int af ce (\a ce' -> return (a, ce'))


runIntEscape ::
  forall ref efs a.
  Af (Escape Int ref : efs) a -> Af efs (Either Int a)
runIntEscape af =
  runEscape @ref @Int af
    (\ a -> return (Right a)) (\ s -> return (Left s))


readHasCell3 :: forall efs. In Cell3 efs => Af efs Int
readHasCell3 = readCell @CellRef3


writeHasCell3 :: forall efs. In Cell3 efs => Af efs ()
writeHasCell3 = writeCell @CellRef3 @Int 30


delimitReadCell3 :: forall efs. In Cell3 efs => Af efs (Int, Int)
delimitReadCell3 =
  delimitCell @CellRef3 readHasCell3 30
    (\ a ce -> return (a, ce)) (\ _ _ -> 123456)


delimitWriteCell3 :: forall efs. In Cell3 efs => Af efs Int
delimitWriteCell3 =
  delimitCell @CellRef3 writeHasCell3 123456
    (\ _ ce -> return ce) (\ _ _ -> 123456)


rotateStates5 ::
  forall efs. AllIn '[Cell4, Cell1, Cell2, Cell5, Cell3] efs =>
  Af efs Int
rotateStates5 = do
  c1 <- readCell @CellRef1 @Int
  c5 <- readCell @CellRef5 @Int
  writeCell @CellRef5 c1
  writeCell @CellRef1 c5
  c3 <- readCell @CellRef3 @Int
  c4 <- readCell @CellRef4 @Int
  c2 <- readCell @CellRef2 @Int
  writeCell @CellRef4 c2
  writeCell @CellRef2 c4
  return (c1 + c2 + c3 + c4 + c5)


delimitCellEscape ::
  forall efs. (In Cell3 efs, In Escape1 efs) => Af efs Int
delimitCellEscape =
  catchEscape @EscapeRef1 @Int (
      delimitCell @CellRef3 (do
          writeHasCell3
          takeEscape_ @EscapeRef1 (123456 :: Int)
          writeCell @CellRef3 @Int 123456
        ) (123456 :: Int) (\ _ _ -> return 123456) (+)
    ) (\ _ -> readHasCell3)


localReadCell3 :: forall efs. In Cell3 efs => Af efs (Int, Int)
localReadCell3 =
  localCell @CellRef3 readHasCell3 30 (\ a ce -> return (a, ce))


localWriteCell3 :: forall efs. In Cell3 efs => Af efs Int
localWriteCell3 =
  localCell @CellRef3 writeHasCell3 123456 (\ _ ce -> return ce)


localCellEscape ::
  forall efs. (In Cell3 efs, In Escape1 efs) => Af efs Int
localCellEscape =
  catchEscape @EscapeRef1 @Int (
      localCell @CellRef3 (do
          writeHasCell3
          takeEscape_ @EscapeRef1 (123456 :: Int)
          writeCell @CellRef3 @Int 123456
        ) (123456 :: Int) (\ _ _ -> return 123456)
    ) (\ _ -> readHasCell3)


cellTest :: IO ()
cellTest = do
  putStrLn "single readHasCell3"
  print $ runAfPure $ runIntCell @CellRef3 3 readHasCell3

  putStrLn "\ncombined readHasCell3"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 readHasCell3

  putStrLn "\nsingle writeHasCell3"
  print $ runAfPure $ runIntCell @CellRef3 3 writeHasCell3

  putStrLn "\ncombined writeHasCell3"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 writeHasCell3

  putStrLn "\nsingle delimitReadCell3"
  print $ runAfPure $ runIntCell @CellRef3 3 delimitReadCell3

  putStrLn "\ncombined delimitReadCell3"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 delimitReadCell3

  putStrLn "\nsingle delimitWriteCell3"
  print $ runAfPure $ runIntCell @CellRef3 3 delimitWriteCell3

  putStrLn "\ncombined delimitWriteCell3"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 delimitWriteCell3

  putStrLn "\nrotateStates5"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 rotateStates5

  putStrLn "\nsingle internal delimitCellEscape"
  print $ runAfPure $
    runIntCell @CellRef3 3 $
    runIntEscape @EscapeRef1 delimitCellEscape

  putStrLn "\ncombined internal delimitCellEscape 0"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntEscape @EscapeRef2 $
    runIntEscape @EscapeRef3 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 $
    runIntEscape @EscapeRef1 delimitCellEscape

  putStrLn "\ncombined internal delimitCellEscape 1"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntEscape @EscapeRef1 $
    runIntCell @CellRef5 5 $
    runIntEscape @EscapeRef3 $
    runIntEscape @EscapeRef2 delimitCellEscape

  putStrLn "\ncombined internal delimitCellEscape 2"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntEscape @EscapeRef3 $
    runIntCell @CellRef3 3 $
    runIntEscape @EscapeRef2 $
    runIntEscape @EscapeRef1 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 delimitCellEscape

  putStrLn "\nsingle external delimitCellEscape"
  print $ runAfPure $
    runIntEscape @EscapeRef1 $
    runIntCell @CellRef3 3 delimitCellEscape

  putStrLn "\ncombined external delimitCellEscape 0"
  print $ runAfPure $
    runIntEscape @EscapeRef1 $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntEscape @EscapeRef3 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntEscape @EscapeRef2 $
    runIntCell @CellRef5 5 delimitCellEscape

  putStrLn "\ncombined external delimitCellEscape 1"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntEscape @EscapeRef1 $
    runIntEscape @EscapeRef3 $
    runIntCell @CellRef2 2 $
    runIntEscape @EscapeRef2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 delimitCellEscape

  putStrLn "\ncombined external delimitCellEscape 2"
  print $ runAfPure $
    runIntEscape @EscapeRef2 $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntEscape @EscapeRef1 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntEscape @EscapeRef3 $
    runIntCell @CellRef5 5 delimitCellEscape

  putStrLn "\nsingle localReadCell3"
  print $ runAfPure $ runIntCell @CellRef3 3 localReadCell3

  putStrLn "\ncombined localReadCell3"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 localReadCell3

  putStrLn "\nsingle localWriteCell3"
  print $ runAfPure $ runIntCell @CellRef3 3 localWriteCell3

  putStrLn "\ncombined localWriteCell3"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 localWriteCell3

  putStrLn "\nsingle internal localCellEscape"
  print $ runAfPure $
    runIntCell @CellRef3 3 $
    runIntEscape @EscapeRef1 localCellEscape

  putStrLn "\ncombined internal localCellEscape"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntEscape @EscapeRef1 $
    runIntCell @CellRef5 5 localCellEscape

  putStrLn "\nsingle external localCellEscape"
  print $ runAfPure $
    runIntEscape @EscapeRef1 $
    runIntCell @CellRef3 3 localCellEscape

  putStrLn "\ncombined external localCellEscape"
  print $ runAfPure $
    runIntCell @CellRef1 1 $
    runIntEscape @EscapeRef1 $
    runIntCell @CellRef2 2 $
    runIntCell @CellRef3 3 $
    runIntCell @CellRef4 4 $
    runIntCell @CellRef5 5 localCellEscape
