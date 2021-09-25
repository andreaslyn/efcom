module EscapeTest.Test (escapeTest) where

import EscapeTest.Util

import Control.Af

import Test.Hspec
  ( it
  , shouldBe
  , SpecWith
  )


singleTakeEscapeTest :: SpecWith ()
singleTakeEscapeTest =
  it "single take escape" $
    test `shouldBe` Left 2
  where
    test = runAfPure $ runIntEscape @EscapeRef2 takeEscape2


combinedTakeEscapeTest :: SpecWith ()
combinedTakeEscapeTest =
  it "combined take escape" $
    test `shouldBe` (Right (Left 2, 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 takeEscape2


combinedDelimitWithNoEscapeTest :: SpecWith ()
combinedDelimitWithNoEscapeTest =
  it "combined delimit with no escape" $
    test `shouldBe` (Right (Right (Right (3, 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 delimitWithNoEscape


combinedDelimitWithTakeEscape0Test :: SpecWith ()
combinedDelimitWithTakeEscape0Test =
  it "combined delimit with take escape 0" $
    test `shouldBe` (Right (Right (Right ((((2, 4), 3), 2), 1))))
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 delimitWithTakeEscape


combinedDelimitWithTakeEscape1Test :: SpecWith ()
combinedDelimitWithTakeEscape1Test =
  it "combined delimit with take escape 1" $
    test `shouldBe` Right (Right (Right (((2, 4), 3), 2), 10))
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 delimitWithTakeEscape


combinedDelimitWithTakeEscape2Test :: SpecWith ()
combinedDelimitWithTakeEscape2Test =
  it "combined delimit with take escape 2" $
    test `shouldBe` ((Right ((Right (Right 2), 4), 3), 20), 10)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef1 $
      runIntEscape @EscapeRef3 $ delimitWithTakeEscape


combinedDelimitWithTakeEscape3Test :: SpecWith ()
combinedDelimitWithTakeEscape3Test =
  it "combined delimit with take escape 3" $
    test `shouldBe` ((Right (Right (Right 2, 4), 30), 20), 10)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef3 $ delimitWithTakeEscape


combinedDelimitWithTakeEscape4Test :: SpecWith ()
combinedDelimitWithTakeEscape4Test =
  it "combined delimit with take escape 4" $
    test `shouldBe` ((Right ((Right (Right 2), 40), 30), 20), 10)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef3 $
      runIntEscape @EscapeRef2 $ delimitWithTakeEscape


nestedDelimitTakeFirstEscape0Test :: SpecWith ()
nestedDelimitTakeFirstEscape0Test =
  it "nested delimit take first escape 0" $
    test `shouldBe` Right (Right (Right ((((1, 4), 3), 2), 1)))
  where
    test = runAfPure $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef3 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape1


nestedDelimitTakeFirstEscape1Test :: SpecWith ()
nestedDelimitTakeFirstEscape1Test =
  it "nested delimit take first escape 1" $
    test `shouldBe` (Right (Right (Right (1, 4), 3), 2), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape1


nestedDelimitTakeFirstEscape2Test :: SpecWith ()
nestedDelimitTakeFirstEscape2Test =
  it "nested delimit take first escape 2" $
    test `shouldBe` (Right (Right (Right (1, 4), 3), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape1


nestedDelimitTakeFirstEscape3Test :: SpecWith ()
nestedDelimitTakeFirstEscape3Test =
  it "nested delimit take first escape 3" $
    test `shouldBe` (((Right (Right (Right (1, 4))), 3000), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef1 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape1


nestedDelimitTakeFirstEscape4Test :: SpecWith ()
nestedDelimitTakeFirstEscape4Test =
  it "nested delimit take first escape 4" $
    test `shouldBe` ((Right ((Right (Right 1), 4000), 3000), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef1 $
      runIntEscape @EscapeRef3 $ nestedDelimitTakeEscape1


nestedDelimitTakeSecondEscape0Test :: SpecWith ()
nestedDelimitTakeSecondEscape0Test =
  it "nested delimit take second escape 0" $
    test `shouldBe` Right (Right (((Right (2, 40), 30), 20), 10))
  where
    test = runAfPure $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape2


nestedDelimitTakeSecondEscape1Test :: SpecWith ()
nestedDelimitTakeSecondEscape1Test =
  it "nested delimit take second escape 1" $
    test `shouldBe` Right (Right ((Right (2, 40), 30), 20), 1000)
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape2


nestedDelimitTakeSecondEscape2Test :: SpecWith ()
nestedDelimitTakeSecondEscape2Test =
  it "nested delimit take second escape 2" $
    test `shouldBe` ((Right (Right (Right 2, 40), 30), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef3 $ nestedDelimitTakeEscape2


nestedDelimitTakeSecondEscape3Test :: SpecWith ()
nestedDelimitTakeSecondEscape3Test =
  it "nested delimit take second escape 3" $
    test `shouldBe` ((Right (Right (Right (2, 40)), 3000), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef4 4 $ nestedDelimitTakeEscape2


nestedDelimitTakeSecondEscape4Test :: SpecWith ()
nestedDelimitTakeSecondEscape4Test =
  it "nested delimit take second escape 4" $
    test `shouldBe` (((Right (Right (Right 2), 4000), 3000), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef1 $ nestedDelimitTakeEscape2


nestedDelimitTakeThirdEscape0Test :: SpecWith ()
nestedDelimitTakeThirdEscape0Test =
  it "nested delimit take third escape 0" $
    test `shouldBe` Right (Right (Right ((((3, 400), 300), 200), 100)))
  where
    test = runAfPure $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef3 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape3


nestedDelimitTakeThirdEscape1Test :: SpecWith ()
nestedDelimitTakeThirdEscape1Test =
  it "nested delimit take third escape 1" $
    test `shouldBe` Right (Right (Right (((3, 400), 300), 200), 1000))
  where
    test = runAfPure $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape3


nestedDelimitTakeThirdEscape2Test :: SpecWith ()
nestedDelimitTakeThirdEscape2Test =
  it "nested delimit take third escape 2" $
    test `shouldBe` ((Right (Right (Right ((3, 400), 300))), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef3 $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 nestedDelimitTakeEscape3


nestedDelimitTakeThirdEscape3Test :: SpecWith ()
nestedDelimitTakeThirdEscape3Test =
  it "nested delimit take third escape 3" $
    test `shouldBe` ((Right (Right (Right 3, 400), 3000), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef1 $ nestedDelimitTakeEscape3


nestedDelimitTakeThirdEscape4Test :: SpecWith ()
nestedDelimitTakeThirdEscape4Test =
  it "nested delimit take third escape 4" $
    test `shouldBe` ((((Right (Right (Right 3)), 4000), 3000), 2000), 1000)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef3 $
      runIntEscape @EscapeRef1 $ nestedDelimitTakeEscape3


delimitFirstRunSecondDelimitSecondEscapeFirst0Test :: SpecWith ()
delimitFirstRunSecondDelimitSecondEscapeFirst0Test =
  it "delimit first run second delimit second escape first 0" $
    test `shouldBe` Right (Right 1, 1)
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $ delimit1Run2Delimit2Escape1


delimitFirstRunSecondDelimitSecondEscapeFirst1Test :: SpecWith ()
delimitFirstRunSecondDelimitSecondEscapeFirst1Test =
  it "delimit first run second delimit second escape first 1" $
    test `shouldBe` (Right (Right 1), 10)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $ delimit1Run2Delimit2Escape1


delimitFirstRunSecondDelimitSecondEscapeSecond0Test :: SpecWith ()
delimitFirstRunSecondDelimitSecondEscapeSecond0Test =
  it "delimit first run second delimit second escape second 0" $
    test `shouldBe` Right (Right 2, 10)
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $ delimit1Run2Delimit2Escape2


delimitFirstRunSecondDelimitSecondEscapeSecond1Test :: SpecWith ()
delimitFirstRunSecondDelimitSecondEscapeSecond1Test =
  it "delimit first run second delimit second escape second 1" $
    test `shouldBe` (Right (Right 2), 10)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $ delimit1Run2Delimit2Escape2


delimitFirstRunSecondDelimitFirstEscapeFirst0Test :: SpecWith ()
delimitFirstRunSecondDelimitFirstEscapeFirst0Test =
  it "delimit first run second delimit first escape first 0" $
    test `shouldBe` Right (Right 1, 10)
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $ delimit1Run2Delimit1Escape1


delimitFirstRunSecondDelimitFirstEscapeFirst1Test :: SpecWith ()
delimitFirstRunSecondDelimitFirstEscapeFirst1Test =
  it "delimit first run second delimit first escape first 1" $
    test `shouldBe` (Right (Right 1), 11)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $ delimit1Run2Delimit1Escape1


delimitFirstRunSecondDelimitFirstEscapeSecond0Test :: SpecWith ()
delimitFirstRunSecondDelimitFirstEscapeSecond0Test =
  it "delimit first run second delimit first escape second 0" $
    test `shouldBe` Right (Left 2, 11)
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $ delimit1Run2Delimit1Escape2


delimitFirstRunSecondDelimitFirstEscapeSecond1Test :: SpecWith ()
delimitFirstRunSecondDelimitFirstEscapeSecond1Test =
  it "delimit first run second delimit first escape second 1" $
    test `shouldBe` (Right (Left 2), 11)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $ delimit1Run2Delimit1Escape2


escapeTest :: SpecWith ()
escapeTest = do
  singleTakeEscapeTest
  combinedTakeEscapeTest
  combinedDelimitWithNoEscapeTest
  combinedDelimitWithTakeEscape0Test
  combinedDelimitWithTakeEscape1Test
  combinedDelimitWithTakeEscape2Test
  combinedDelimitWithTakeEscape3Test
  combinedDelimitWithTakeEscape4Test
  nestedDelimitTakeFirstEscape0Test
  nestedDelimitTakeFirstEscape1Test
  nestedDelimitTakeFirstEscape2Test
  nestedDelimitTakeFirstEscape3Test
  nestedDelimitTakeFirstEscape4Test
  nestedDelimitTakeSecondEscape0Test
  nestedDelimitTakeSecondEscape1Test
  nestedDelimitTakeSecondEscape2Test
  nestedDelimitTakeSecondEscape3Test
  nestedDelimitTakeSecondEscape4Test
  nestedDelimitTakeThirdEscape0Test
  nestedDelimitTakeThirdEscape1Test
  nestedDelimitTakeThirdEscape2Test
  nestedDelimitTakeThirdEscape3Test
  nestedDelimitTakeThirdEscape4Test
  delimitFirstRunSecondDelimitSecondEscapeFirst0Test
  delimitFirstRunSecondDelimitSecondEscapeFirst1Test
  delimitFirstRunSecondDelimitSecondEscapeSecond0Test
  delimitFirstRunSecondDelimitSecondEscapeSecond1Test
  delimitFirstRunSecondDelimitFirstEscapeFirst0Test
  delimitFirstRunSecondDelimitFirstEscapeFirst1Test
  delimitFirstRunSecondDelimitFirstEscapeSecond0Test
  delimitFirstRunSecondDelimitFirstEscapeSecond1Test
