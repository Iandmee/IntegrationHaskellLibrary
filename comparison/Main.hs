{-# LANGUAGE ExtendedDefaultRules #-}
module Main (main) where

import           Expr
import           Graphics.Matplotlib
import qualified Integration         as I


inputFunction :: Double -> Expr
inputFunction x = Sqrt (Val x) 2

min_eps = 1 / (exp 10)
start_eps = 1.0
bound_left = 1.0
bound_right = 10.0
epsOfError = 1e-8
arrayX = [1..(round $ log $ (start_eps / min_eps))]

getMethodresults :: (Input -> Either Error Output) -> Input -> Double -> [Double]
getMethodresults partXXX input min_epsilon = do
  if (abs ( (eps input) - min_epsilon) <= epsOfError)
    then []
  else case (partXXX input) of
	(Left ex) -> []
	(Right res) -> (result res) : (getMethodresults partXXX (Input {f = (f input), a = (a input), b = (b input), eps = ( (eps input) / (exp 1))}) min_epsilon)

input = Input {f = inputFunction, a = bound_left, b = bound_right, eps = start_eps}
main = file "comparison" $
  plot arrayX (getMethodresults I.partApproxReactangles input min_eps) @@ [o1 "go-",o2 "label" "Reactangles", o2 "linewidth" 2] %
  plot arrayX (getMethodresults I.partApproxTrap input min_eps) @@ [o1 "ro-",o2 "label" "Traps", o2 "linewidth" 2] %
  plot arrayX (getMethodresults I.partApproxSimpson input min_eps) @@ [o1 "bo-",o2 "label" "Simpson", o2 "linewidth" 2]
  % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "upper left"]
