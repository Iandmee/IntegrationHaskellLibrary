module Main (main) where
import Integration

f1 :: Double -> Double
f1 x = (x^2)

f2 :: Double -> Double
f2 x = 1.0 / x 

main :: IO()
main = do 
  r <- partApproxReactangles f1 1.0 2.0 0.001
  putStrLn $ show $ r
  r <- partApproxTrap f1 1.0 2.0 0.001
  putStrLn $ show $ r
  r <- partApproxSimpson f1 1.0 2.0 0.001
  putStrLn $ show $ r