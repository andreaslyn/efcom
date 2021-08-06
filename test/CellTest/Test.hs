module CellTest.Test (cellTest) where

import CellTest.Util

import Control.Af

import Test.Hspec
  ( it
  , shouldBe
  , SpecWith
  )


singleReadCellTest :: SpecWith ()
singleReadCellTest =
  it "single read cell" $
    test `shouldBe` (3, 3)
  where
    test = runAfPure $ runIntCell @CellRef3 3 readHasCell3


combinedReadCellTest :: SpecWith ()
combinedReadCellTest =
  it "combined read cell" $
    test `shouldBe` (((((3, 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 readHasCell3


singleWriteCellTest :: SpecWith ()
singleWriteCellTest =
  it "single write cell" $
    test `shouldBe` ((), 30)
  where
    test = runAfPure $ runIntCell @CellRef3 3 writeHasCell3


combinedWriteCellTest :: SpecWith ()
combinedWriteCellTest =
  it "combined write cell" $
    test `shouldBe` ((((((), 5), 4), 30), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 writeHasCell3


singleDelimitReadCellTest :: SpecWith ()
singleDelimitReadCellTest =
  it "single delimit read cell" $
    test `shouldBe` ((30, 30), 3)
  where
    test = runAfPure $ runIntCell @CellRef3 3 delimitReadCell3


combinedDelimitReadCellTest :: SpecWith ()
combinedDelimitReadCellTest =
  it "combined delimit read cell" $
    test `shouldBe` ((((((30, 30), 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 delimitReadCell3


singleDelimitWriteCellTest :: SpecWith ()
singleDelimitWriteCellTest =
  it "single delimit write cell" $
    test `shouldBe` (30, 3)
  where
    test = runAfPure $ runIntCell @CellRef3 3 delimitWriteCell3


combinedDelimitWriteCellTest :: SpecWith ()
combinedDelimitWriteCellTest =
  it "combined delimit write cell" $
    test `shouldBe` (((((30, 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 delimitWriteCell3


rotateStatesTest :: SpecWith ()
rotateStatesTest =
  it "rotate states" $
    test `shouldBe` (((((15, 1), 2), 3), 4), 5)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 rotateStates5


singleDelimitCellWithInternalEscapeTest :: SpecWith ()
singleDelimitCellWithInternalEscapeTest =
  it "single delimit cell with internal escape" $
    test `shouldBe` (Right 33, 33)
  where
    test = runAfPure $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef1 delimitCellWithEscape


combinedDelimitCellWithInternalEscape0Test :: SpecWith ()
combinedDelimitCellWithInternalEscape0Test =
  it "combined delimit cell with internal escape 0" $
    test `shouldBe` (Right (Right ((((Right 33, 5), 4), 33), 2)), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 $
      runIntEscape @EscapeRef1 delimitCellWithEscape


combinedDelimitCellWithInternalEscape1Test :: SpecWith ()
combinedDelimitCellWithInternalEscape1Test =
  it "combined delimit cell with internal escape 1" $
    test `shouldBe` ((((Right (Right (Right 33), 5), 4), 33), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef5 5 $
      runIntEscape @EscapeRef3 $
      runIntEscape @EscapeRef2 delimitCellWithEscape


combinedDelimitCellWithInternalEscape2Test :: SpecWith ()
combinedDelimitCellWithInternalEscape2Test =
  it "combined delimit cell with internal escape 2" $
    test `shouldBe` ((Right (Right (Right ((33, 5), 4)), 33), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 delimitCellWithEscape


singleDelimitCellWithExternalEscapeTest :: SpecWith ()
singleDelimitCellWithExternalEscapeTest =
  it "single delimit cell with external escape" $
    test `shouldBe` Right (3, 3)
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef3 3 delimitCellWithEscape


combinedDelimitCellWithExternalEscape0Test :: SpecWith ()
combinedDelimitCellWithExternalEscape0Test =
  it "combined delimit cell with external escape 0" $
    test `shouldBe` Right ((Right ((Right (3, 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef5 5 delimitCellWithEscape


combinedDelimitCellWithExternalEscape1Test :: SpecWith ()
combinedDelimitCellWithExternalEscape1Test =
  it "combined delimit cell with external escape 1" $
    test `shouldBe` (Right (Right (Right (((3, 5), 4), 3), 2)), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 delimitCellWithEscape


combinedDelimitCellWithExternalEscape2Test :: SpecWith ()
combinedDelimitCellWithExternalEscape2Test =
  it "combined delimit cell with external escape 2" $
    test `shouldBe` Right ((Right ((Right (3, 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntEscape @EscapeRef2 $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef3 $
      runIntCell @CellRef5 5 delimitCellWithEscape


singleLocalReadCellTest :: SpecWith ()
singleLocalReadCellTest =
  it "single local read cell" $
    test `shouldBe` ((30, 30), 3)
  where
    test = runAfPure $ runIntCell @CellRef3 3 localReadCell3


combinedLocalReadCellTest :: SpecWith ()
combinedLocalReadCellTest =
  it "combined local read cell" $
    test `shouldBe` ((((((30, 30), 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 localReadCell3


singleLocalWriteCellTest :: SpecWith ()
singleLocalWriteCellTest =
  it "single local write cell" $
    test `shouldBe` (30, 3)
  where
    test = runAfPure $ runIntCell @CellRef3 3 localWriteCell3


combinedLocalWriteCellTest :: SpecWith ()
combinedLocalWriteCellTest =
  it "combined local write cell" $
    test `shouldBe` (((((30, 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 localWriteCell3


singleLocalCellWithInternalEscape :: SpecWith ()
singleLocalCellWithInternalEscape =
  it "single local cell with internal escape" $
    test `shouldBe` (Right 3, 3)
  where
    test = runAfPure $
      runIntCell @CellRef3 3 $
      runIntEscape @EscapeRef1 localCellWithEscape


combinedLocalCellWithInternalEscape :: SpecWith ()
combinedLocalCellWithInternalEscape =
  it "combined local cell with internal escape" $
    test `shouldBe` ((((Right (3, 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef5 5 localCellWithEscape


singleLocalCellWithExternalEscape :: SpecWith ()
singleLocalCellWithExternalEscape =
  it "single local cell with external escape" $
    test `shouldBe` Right (3, 3)
  where
    test = runAfPure $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef3 3 localCellWithEscape


combinedLocalCellWithExternalEscape :: SpecWith ()
combinedLocalCellWithExternalEscape =
  it "combined local cell with external escape" $
    test `shouldBe` (Right ((((3, 5), 4), 3), 2), 1)
  where
    test = runAfPure $
      runIntCell @CellRef1 1 $
      runIntEscape @EscapeRef1 $
      runIntCell @CellRef2 2 $
      runIntCell @CellRef3 3 $
      runIntCell @CellRef4 4 $
      runIntCell @CellRef5 5 localCellWithEscape


cellTest :: SpecWith ()
cellTest = do
  singleReadCellTest
  combinedReadCellTest
  singleWriteCellTest
  combinedWriteCellTest
  singleDelimitReadCellTest
  combinedDelimitReadCellTest
  singleDelimitWriteCellTest
  combinedDelimitWriteCellTest
  rotateStatesTest
  singleDelimitCellWithInternalEscapeTest
  combinedDelimitCellWithInternalEscape0Test
  combinedDelimitCellWithInternalEscape1Test
  combinedDelimitCellWithInternalEscape2Test
  singleDelimitCellWithExternalEscapeTest
  combinedDelimitCellWithExternalEscape0Test
  combinedDelimitCellWithExternalEscape1Test
  combinedDelimitCellWithExternalEscape2Test
  singleLocalReadCellTest
  combinedLocalReadCellTest
  singleLocalWriteCellTest
  combinedLocalWriteCellTest
  singleLocalCellWithInternalEscape
  combinedLocalCellWithInternalEscape
  singleLocalCellWithExternalEscape
  combinedLocalCellWithExternalEscape
