-- {-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}

import qualified Test.PropertyBasedTest
import           Test.Tasty
import           Test.Tasty.Hspec
import qualified Test.UnitTest
main :: IO ()
main = do
  defaultMain (testGroup "All"
                [ testGroup	"Properties" Test.PropertyBasedTest.props
                , testGroup "Units" Test.UnitTest.units
                ])
