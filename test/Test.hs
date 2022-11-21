-- {-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}

import Test.Tasty
import Test.Tasty.Hspec
import qualified Test.PropertyBasedTest
import qualified Test.UnitTest
main :: IO ()
main = do
  defaultMain (testGroup "All"
                [ testGroup	"Properties" Test.PropertyBasedTest.props
                , testGroup "Units" Test.UnitTest.units
                ])