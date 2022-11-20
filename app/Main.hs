module Main (main) where
import Integration
import Expr

f1 :: Double -> Expr
f1 x = Bin Mul (Val x) (Val x)

f2 :: Double -> Expr
f2 x = Bin Div (Val 1) (Val x)

main :: IO()
main = do
  putStrLn $ show $ (1/0 - 1/0)
  let r = partApproxReactangles f1 1.0 2.0 0.001
  putStrLn $ show $ r
  let r = partApproxTrap f1 1.0 2.0 0.001
  putStrLn $ show $ r
  let r = partApproxSimpson f1 1.0 2.0 0.001
  putStrLn $ show $ r