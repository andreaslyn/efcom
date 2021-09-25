module CellTest.Util where

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


delimitCellWithEscape ::
  forall efs. (In Cell3 efs, In Escape1 efs) => Af efs Int
delimitCellWithEscape =
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


localCellWithEscape ::
  forall efs. (In Cell3 efs, In Escape1 efs) => Af efs Int
localCellWithEscape =
  catchEscape @EscapeRef1 @Int (
      localCell @CellRef3 (do
          writeHasCell3
          takeEscape_ @EscapeRef1 (123456 :: Int)
          writeCell @CellRef3 @Int 123456
        ) (123456 :: Int) (\ _ _ -> return 123456)
    ) (\ _ -> readHasCell3)
