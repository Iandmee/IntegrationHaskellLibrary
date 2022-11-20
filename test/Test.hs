{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}

import Test.Tasty
import Test.Tasty.Hspec

import qualified Test.PropertyBasedTest

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ asdasd
                ])