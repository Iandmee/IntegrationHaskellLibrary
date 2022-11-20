module Test.PropertyBasedTest where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Expr
import Integration
import Test.UnitTest (checkAllmethods, checkEquality)

genOp :: Gen BinOp
genOp = Gen.element [Sum, Sub, Mul, Div]

genDouble :: Double -> Gen Double
genDouble n = Gen.double (Range.constant 1 n)

genExpr :: Double -> Gen Expr
genExpr n =
  Gen.recursive
    Gen.choice
    [ -- нерекурсивные генераторы
      numGen
    ]
    [ -- рекурсивные генераторы
      binOpGen
    ]
  where
    numGen = Val <$> Gen.double (Range.constant 1 n)
    binOpGen = do
      op <- genOp
      Gen.subterm2 (genExpr	n) (genExpr n) (Bin op)


checkEqualityBool :: (Double -> Expr) -> Double -> Double -> Error -> Bool
checkEqualityBool ex bound eps er = (partApproxSimpson ex bound (bound + 1) eps) == (Left er) && (partApproxTrap ex bound (bound + 1) eps) == (Left er) && (partApproxReactangles ex bound (bound + 1) eps) == (Left er)

prop_DivisionByZero :: Property
prop_DivisionByZero = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityBool (\x -> Bin Sum (expr) (Bin Div (Val x) (Val 0))) bound 0.001 DivisionByZero)

prop_LogOfZero :: Property
prop_LogOfZero = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityBool (\x -> Bin Sum (expr) (Bin Sub (Un Log (Val 0)) (Val x))) bound 0.001 LogOfZero)

prop_LogOfNegativeNumber :: Property
prop_LogOfNegativeNumber = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityBool (\x -> Bin Sum (expr) (Bin Sub (Un Log (Val (-2))) (Val x))) bound 0.001 LogOfNegativeNumber)

prop_SqrtOfNegativeNumber :: Property
prop_SqrtOfNegativeNumber = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityBool (\x -> Bin Sum (expr) (Bin Sub (Sqrt (Val (-2)) 2) (Val x))) bound 0.001 SqrtOfNegativeNumber)

prop_SqrtWithSmallDegree :: Property
prop_SqrtWithSmallDegree = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityBool (\x -> Bin Sum (expr) (Bin Sub (Sqrt (Val 2) (-1)) (Val x))) bound 0.001 SqrtWithSmallDegree)

prop_NullSizeOfError :: Property
prop_NullSizeOfError = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityBool (\x -> Bin Sum (expr) (Bin Sub (Un Log (Val 2)) (Val x))) bound 0.0 NullSizeOfError)

prop_SomeIntegralError :: Property
prop_SomeIntegralError = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityBool (\x -> Bin Sum (expr) (Bin Sub (Un Exp (Val 1000000.0)) (Val x))) bound 0.001 SomeIntegralError)



props :: [TestTree]
props =
  [ testProperty "DivisionByZero error" prop_DivisionByZero
  , testProperty "LogOfZero error" prop_LogOfZero
  , testProperty "LogOfNegativeNumber error" prop_LogOfNegativeNumber
  , testProperty "SqrtOfNegativeNumber error" prop_SqrtOfNegativeNumber
  , testProperty "SqrtWithSmallDegree error" prop_SqrtWithSmallDegree
  , testProperty "NullSizeOfError error" prop_NullSizeOfError
  , testProperty "SomeIntegralError error" prop_SomeIntegralError
  ]


