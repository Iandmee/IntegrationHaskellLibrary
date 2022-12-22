{-# LANGUAGE ImplicitParams #-}

module Test.UnitTest where

import           Expr
import           Hedgehog
import qualified Integration         as I
import           Test.HUnit.Approx   ((@?~))
import           Test.Tasty
import           Test.Tasty.HUnit    (Assertion, assertBool, testCase, (@?=))
import           Test.Tasty.Hedgehog

f1 :: Double -> Expr
f1 x = Bin Mul (Val x) (Val x)

f2 :: Double -> Expr
f2 x = Un Log (Val x)

f3 :: Double -> Expr
f3 x = Sqrt (Val x) 2

f4 :: Double -> Expr
f4 x = Bin Div (Val x) (Val 0.0)

f5 :: Double -> Expr
f5 x = Un Exp (Val x)

f6 :: Double -> Expr
f6 x = Sqrt (Val x) 1

f7 :: Double -> Expr
f7 x = Bin Sum (Un Log (Val 0.0)) (Val x)

checkEquality :: Either Error Output -> Either Error Double -> Double -> Assertion
checkEquality x y eps =
    let ?epsilon = eps in
    go x y
    where
	  go (Right x) (Right ans) = do
		(result x) @?~ (ans)
	  go (Left x) (Left y) = do
	  	x @?= y
	  go _ _ = do
	  	fail $ "Extra Error!"


checkAllmethods :: (Double -> Expr) -> Double -> Double -> Either Error Double -> Double -> Assertion
checkAllmethods f a b errorOccured eps = do
   let res = I.partApproxReactangles $ Input {f=f, a=a, b=b, eps=eps}
   checkEquality res errorOccured eps
   let res = I.partApproxTrap $ Input {f=f, a=a, b=b, eps=eps}
   checkEquality res errorOccured eps
   let res = I.partApproxSimpson $ Input {f=f, a=a, b=b, eps=eps}
   checkEquality res errorOccured eps

unit_ValidIntegrations = do
  checkAllmethods f1 1.0 2.0 (Right (7/3)) 0.001
  checkAllmethods f2 1.0 5.0 (Right 4.04718956217) 0.001
  checkAllmethods f3 1000.0 2000.0 (Right 38546.62833220519635758534) 0.0001

unit_InvalidIntegrations = do
  checkAllmethods f1 1.0 5.0 (Left NullSizeOfError) 0.0
  checkAllmethods f2 (-1.0) 2.0 (Left LogOfNegativeNumber) 0.001
  checkAllmethods f3 (-2.0) 2.0 (Left SqrtOfNegativeNumber) 0.001
  checkAllmethods f4 (100.0) (1000.0) (Left DivisionByZero) 0.001
  checkAllmethods f5 (1.0) (100000000000.0) (Left SomeIntegralError) 0.00001
  checkAllmethods f6 1.0 10.0 (Left SqrtWithSmallDegree) 0.001
  checkAllmethods f7 1.0 10.0 (Left LogOfZero) 0.001

units :: [TestTree]
units = [testCase "Valid" unit_ValidIntegrations, testCase "Invalid" unit_InvalidIntegrations]
