module Test.PropertyBasedTest where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Expr
import Test.UnitTest (checkAllmethods, checkEquality)

genInt :: Gen Int
genInt = Gen.int (Range.constant 5 100)

generateExprByResult :: Either Error Double -> (Double -> Expr)
generateExprByResult e  = case e of 
                       Left LogOfZero -> do 
                       	t <- genInt
                       	( \x -> Sum (Val x) (Log (Div (Val 0) (Val (fromIntegral t)))) )
                       Left LogOfNegativeNumber -> do 
                       	t <- genInt
                       	( \x -> Sum (Val x) (Log ( Div (Exp (Log (Val (fromIntegral t)))) (Val (fromIntegral -t)))) )
                       Left SqrtWithSmallDegree -> do 
                       	t <- genInt
                       	( \x -> Sqrt (Mul (Val x)  (Sum (Val 1) (Val 5))) -t ) 
                       Left SqrtOfNegativeNumber -> do 
                       	t <- genInt
                       	( \x -> Sub (Val x) (Sqrt (Val (fromIntegral t)) 2) )
                       Left DivisionByZero -> do 
                       	t <- genInt
                       	( \x -> Div (Sub (Val x) (Val (fromIntegral t))) (Val 0) ) 
                       Right ex -> do 
                       	t <- genInt
                       	( \x -> Sub (Sum (Val x) (Div (Mul (Val ex) (Val (fromIntegral t))) (Exp (Log (Val(fromIntegral t)))))) (Val x) )

prop_LogOfZero :: Property
prop_LogOfZero = property $ do 
	expr <- forAll $ generateExprByResult (Left LogOfNegativeNumber)
	checkAllmethods expr 1.0 2.0 (Just LogOfNegativeNumber) 0.001


