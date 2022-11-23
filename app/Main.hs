module Main (main) where
import           Expr
import qualified Integration as I

f1 :: Double -> Expr
f1 x = Bin Mul (Val x) (Val x)

f2 :: Double -> Expr
f2 x = Bin Div (Val 1) (Val x)

main :: IO()
main = do
  putStrLn $ show $ (1/0 - 1/0)
  let r = I.partApproxReactangles $ Input {f = f1, a = 1.0, b = 2.0, eps = 0.001}
  putStrLn $ show $ r
  let r = I.partApproxTrap $ Input {f = f1, a = 1.0, b = 2.0, eps = 0.001}
  putStrLn $ show $ r
  let r = I.partApproxSimpson $ Input {f = f1, a = 1.0, b = 2.0, eps = 0.001}
  putStrLn $ show $ r
