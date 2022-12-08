module Test.PropertyBasedTest where

import           Expr
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import qualified Integration         as I
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.UnitTest       (checkAllmethods, checkEquality, f1, f2, f3)

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


checkEqualityErrorBool :: (Double -> Expr) -> Double -> Double -> Error -> Bool
checkEqualityErrorBool ex bound eps er = do
  let input = Input {f = ex, a = bound, b = (bound + 1), eps = eps}
  ((I.partApproxSimpson input) == (Left er)) && ((I.partApproxTrap input) == (Left er)) && ((I.partApproxReactangles input) == (Left er))


checkEqualityValidBool :: Input -> Either Error Bool
checkEqualityValidBool input = do
  let e = eps input
  r1 <- I.partApproxSimpson input
  r2 <- I.partApproxReactangles input
  r3 <- I.partApproxTrap input
  return $ (abs ((result r1) - (result r2)) < 2*e) && (abs ((result r1) - (result r3)) < 2*e) && (abs ((result r2) - (result r3)) < 2 * e)


prop_DivisionByZero :: Property
prop_DivisionByZero = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityErrorBool (\x -> Bin Sum (expr) (Bin Div (Val x) (Val 0))) bound 0.001 DivisionByZero)

prop_LogOfZero :: Property
prop_LogOfZero = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityErrorBool (\x -> Bin Sum (expr) (Bin Sub (Un Log (Val 0)) (Val x))) bound 0.001 LogOfZero)

prop_LogOfNegativeNumber :: Property
prop_LogOfNegativeNumber = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityErrorBool (\x -> Bin Sum (expr) (Bin Sub (Un Log (Val (-2))) (Val x))) bound 0.001 LogOfNegativeNumber)

prop_SqrtOfNegativeNumber :: Property
prop_SqrtOfNegativeNumber = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityErrorBool (\x -> Bin Sum (expr) (Bin Sub (Sqrt (Val (-2)) 2) (Val x))) bound 0.001 SqrtOfNegativeNumber)

prop_SqrtWithSmallDegree :: Property
prop_SqrtWithSmallDegree = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityErrorBool (\x -> Bin Sum (expr) (Bin Sub (Sqrt (Val 2) (-1)) (Val x))) bound 0.001 SqrtWithSmallDegree)

prop_NullSizeOfError :: Property
prop_NullSizeOfError = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityErrorBool (\x -> Bin Sum (expr) (Bin Sub (Un Log (Val 2)) (Val x))) bound 0.0 NullSizeOfError)

prop_SomeIntegralError :: Property
prop_SomeIntegralError = property $ do
  expr <- forAll $ genExpr 100
  bound <- forAll $ genDouble 100
  assert (checkEqualityErrorBool (\x -> Bin Sum (expr) (Bin Sub (Un Exp (Val 1000000.0)) (Val x))) bound 0.001 SomeIntegralError)

prop_CheckValid :: Property
prop_CheckValid = property $ do 
  bound1 <- forAll $ genDouble 100
  bound2 <- forAll $ genDouble 100
  let input = Input {f = f1, a = (min bound1 bound2), b = (max bound1 bound2), eps = 0.0001}
  assert ((checkEqualityValidBool input) == (Right True)
    && (checkEqualityValidBool $ input {f = f2}) == (Right True)
    && (checkEqualityValidBool $ input {f = f3}) == (Right True))

props :: [TestTree]
props =
  [ testProperty "DivisionByZero error" prop_DivisionByZero
  , testProperty "LogOfZero error" prop_LogOfZero
  , testProperty "LogOfNegativeNumber error" prop_LogOfNegativeNumber
  , testProperty "SqrtOfNegativeNumber error" prop_SqrtOfNegativeNumber
  , testProperty "SqrtWithSmallDegree error" prop_SqrtWithSmallDegree
  , testProperty "NullSizeOfError error" prop_NullSizeOfError
  , testProperty "SomeIntegralError error" prop_SomeIntegralError
  , testProperty "Check Valid Expressions" prop_CheckValid
  ]


