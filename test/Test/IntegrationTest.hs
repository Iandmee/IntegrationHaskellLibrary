{-# LANGUAGE ImplicitParams #-}

module Test.IntegrationTest where

import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Test.HUnit.Approx ((@?~))

import Integration

f1 :: Double -> Double
f1 x = x ^ 2

integralf1 :: Double -> Double
integralf1 x = x ^ 3 / 3.0

f2 :: Double -> Double
f2 x = 1 / (x - 1.0)

f3 :: Double -> Double
f3 x = 1 / (x - 5.0)

f4 :: Double -> Double
f4 x = x / sin 0.0


checkEquality :: Either Error Double -> Either Error Double -> Double -> Assertion
checkEquality x y eps =
    let ?epsilon = eps in
    go x y
    where
	  go (Right x) (Right y) = do
		x @?~ y
	  go (Left x) (Left y) = do 
	  	x @?= y
	  go _ _ = do
	  	fail $ "Extra Error!"


checkAllmethods :: (Double -> Double) -> Double -> Double -> Either Error Double -> Double -> Assertion
checkAllmethods f a b ans eps = do 
   res <- partApproxReactangles f a b eps
   checkEquality res ans eps
   res <- partApproxTrap f a b eps
   checkEquality res ans eps
   res <- partApproxSimpson f a b eps
   checkEquality res ans eps

unit_ValidIntegrations = do
	checkAllmethods f1 1.0 2.0 (Right ((integralf1 2.0) - (integralf1 1.0))) 0.001

unit_InvalidIntegrations = do 
	checkAllmethods f2 1.0 2.0 (Left InputFunctionOrBoundsError) 0.001