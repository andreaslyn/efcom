module EscapeTest.Util where

import Control.Af
import Control.Af.Cell
import Control.Af.Escape


data EscapeRef1
type Escape1 = Escape Int EscapeRef1

data EscapeRef2
type Escape2 = Escape Int EscapeRef2

data EscapeRef3
type Escape3 = Escape Int EscapeRef3

data CellRef1
type Cell1 = Cell Int CellRef1

data CellRef2
type Cell2 = Cell Int CellRef2

data CellRef3
type Cell3 = Cell Int CellRef3

data CellRef4
type Cell4 = Cell Int CellRef4


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


takeEscape2 :: forall efs. In Escape2 efs => Af efs ()
takeEscape2 = takeEscape @EscapeRef2 @Int 2


delimitWithNoEscape :: forall efs. (In Cell3 efs, In Escape2 efs) => Af efs Int
delimitWithNoEscape =
  delimitEscape @EscapeRef2 @Int (readCell @CellRef3)
    return (\_ -> return 123456)


delimitWithTakeEscape ::
  forall efs. AllIn '[Cell1, Cell2, Cell3, Cell4, Escape2] efs => Af efs Int
delimitWithTakeEscape =
  delimitEscape @EscapeRef2 @Int (do
    writeCell @CellRef1 @Int 10
    writeCell @CellRef2 @Int 20
    writeCell @CellRef3 @Int 30
    writeCell @CellRef4 @Int 40
    takeEscape_ @EscapeRef2 @Int 2
  ) (\ _ -> return 123456) return


nestedDelimitTakeEscape1 ::
  forall efs.
  AllIn '[Cell1, Cell2, Cell3, Cell4
        , Escape1, Escape2, Escape3] efs =>
  Af efs Int
nestedDelimitTakeEscape1 =
  delimitEscape @EscapeRef1 @Int (do
      writeCell @CellRef1 @Int 10
      writeCell @CellRef2 @Int 20
      writeCell @CellRef3 @Int 30
      writeCell @CellRef4 @Int 40
      catchEscape @EscapeRef2 @Int (do
          writeCell @CellRef1 @Int 100
          writeCell @CellRef2 @Int 200
          writeCell @CellRef3 @Int 300
          writeCell @CellRef4 @Int 400
          delimitEscape @EscapeRef3 @Int (do
              writeCell @CellRef1 @Int 1000
              writeCell @CellRef2 @Int 2000
              writeCell @CellRef3 @Int 3000
              writeCell @CellRef4 @Int 4000
              takeEscape_ @EscapeRef1 @Int 1
            ) (\ _ -> return (123456 :: Int)) (\ _ -> return 123456)
        ) (\ _ -> return 123456)
    ) (\ _ -> return 123456) return


nestedDelimitTakeEscape2 ::
  forall efs.
  AllIn '[Cell1, Cell2, Cell3, Cell4
        , Escape1, Escape2, Escape3] efs =>
  Af efs Int
nestedDelimitTakeEscape2 =
  delimitEscape @EscapeRef1 @Int (do
      writeCell @CellRef1 @Int 10
      writeCell @CellRef2 @Int 20
      writeCell @CellRef3 @Int 30
      writeCell @CellRef4 @Int 40
      catchEscape @EscapeRef2 @Int (do
          writeCell @CellRef1 @Int 100
          writeCell @CellRef2 @Int 200
          writeCell @CellRef3 @Int 300
          writeCell @CellRef4 @Int 400
          delimitEscape @EscapeRef3 @Int (do
              writeCell @CellRef1 @Int 1000
              writeCell @CellRef2 @Int 2000
              writeCell @CellRef3 @Int 3000
              writeCell @CellRef4 @Int 4000
              takeEscape @EscapeRef2 @Int 2
            ) (\ _ -> return 123456) (\ _ -> return 123456)
        ) return
    ) return (\ _ -> return 123456)


nestedDelimitTakeEscape3 ::
  forall efs.
  AllIn '[Cell1, Cell2, Cell3, Cell4
        , Escape1, Escape2, Escape3] efs =>
  Af efs Int
nestedDelimitTakeEscape3 =
  delimitEscape @EscapeRef1 @Int (do
      writeCell @CellRef1 @Int 10
      writeCell @CellRef2 @Int 20
      writeCell @CellRef3 @Int 30
      writeCell @CellRef4 @Int 40
      catchEscape @EscapeRef2 @Int (do
          writeCell @CellRef1 @Int 100
          writeCell @CellRef2 @Int 200
          writeCell @CellRef3 @Int 300
          writeCell @CellRef4 @Int 400
          delimitEscape @EscapeRef3 @Int (do
              writeCell @CellRef1 @Int 1000
              writeCell @CellRef2 @Int 2000
              writeCell @CellRef3 @Int 3000
              writeCell @CellRef4 @Int 4000
              takeEscape @EscapeRef3 @Int 3
            ) (\ _ -> return 123456) return
        ) (\ _ -> return 123456)
    ) return (\ _ -> return 123456)


delimit1Run2Delimit2Escape1 ::
  forall efs. (In Cell1 efs, In Escape1 efs) =>
  Af efs (Either Int Int)
delimit1Run2Delimit2Escape1 =
  delimitEscape @EscapeRef1 @Int (do
    runIntEscape @EscapeRef2 $ do
      writeCell @CellRef1 @Int 10
      delimitEscape @EscapeRef2 @Int (do
          takeEscape @EscapeRef1 @Int 1
        ) (\ _ -> return (123456 :: Int)) (\ _ -> return 123456)
  ) (\ _ -> return (Right 123456)) (return . Right)


delimit1Run2Delimit2Escape2 ::
  forall efs. (In Cell1 efs, In Escape1 efs) =>
  Af efs (Either Int Int)
delimit1Run2Delimit2Escape2 =
  delimitEscape @EscapeRef1 @Int (do
    runIntEscape @EscapeRef2 $ do
      writeCell @CellRef1 @Int 10
      delimitEscape @EscapeRef2 @Int (do
          takeEscape @EscapeRef2 @Int 2
        ) (\ _ -> return 123456) return
  ) return (\ _ -> return (Right 123345))


delimit1Run2Delimit1Escape1 ::
  forall efs. (In Cell1 efs, In Escape1 efs) =>
  Af efs (Either Int Int)
delimit1Run2Delimit1Escape1 =
  delimitEscape @EscapeRef1 @Int (do
    runIntEscape @EscapeRef2 $ do
      c1 <- readCell @CellRef1 @Int
      writeCell @CellRef1 @Int 10
      delimitEscape @EscapeRef1 @Int (do
          c2 <- readCell @CellRef1 @Int
          writeCell @CellRef1 @Int (c1 + c2)
          takeEscape @EscapeRef1 @Int 1
        ) (\ _ -> return 123456) return
  ) return (\ _ -> return (Right 123456))


delimit1Run2Delimit1Escape2 ::
  forall efs. (In Cell1 efs, In Escape1 efs) =>
  Af efs (Either Int Int)
delimit1Run2Delimit1Escape2 =
  delimitEscape @EscapeRef1 @Int (do
    runIntEscape @EscapeRef2 $ do
      c1 <- readCell @CellRef1 @Int
      writeCell @CellRef1 @Int 10
      delimitEscape @EscapeRef1 @Int (do
          c2 <- readCell @CellRef1 @Int
          writeCell @CellRef1 @Int (c1 + c2)
          takeEscape @EscapeRef2 @Int 2
        ) (\ _ -> return 123456) (\ _ -> return 123456)
  ) return (\ _ -> return (Right 123456))
