module Main (main) where
import Integration

f :: Double -> Maybe Double
f x = Just (x^2)

main :: IO()
main = do 
  putStrLn $ show $ partApproxReactangles f 1.0 2.0 0.001