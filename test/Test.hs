-- {-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}

import Test.Tasty
import Test.Tasty.Hspec
import Utils
import qualified Test.PropertyBasedTest
import qualified Test.UnitTest
main :: IO ()
main = do
  defaultMainWithOpts (testGroup "All"
                [ testGroup	"Properties" Test.PropertyBasedTest.props
                ])