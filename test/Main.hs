import Test.Hspec
  ( hspec
  , describe
  )
import CellTest.Test
import EscapeTest.Test


main :: IO ()
main = hspec $ do
  describe "Cell Test" cellTest
  describe "Escape Test" escapeTest
