module Integration where

data Error = InputFunctionOrBoundsError
           | NullSizeOfError
           deriving (Show, Eq)

iterateThroughPartition :: ((Double -> Maybe Double) -> Double -> Double -> Double -> Int -> Either Error Double) -> (Double -> Maybe Double) -> Double -> Double -> Double -> Double -> Maybe Double -> Int -> Either Error Double
iterateThroughPartition calculateByXXX f a b d initIntegration lastIntegration parts = do 
  let step = (b - a) / (fromIntegral parts)
  let integration = calculateByXXX f a step (initIntegration * step) parts
  case integration of 
    Left e -> Left e
    Right result -> case lastIntegration of 
      Nothing -> iterateThroughPartition calculateByXXX f a b d initIntegration (Just result) (parts + 1)
      Just lastResult -> if (abs (result - lastResult) <= d) then Right result else iterateThroughPartition calculateByXXX f a b d initIntegration (Just result) (parts + 1)
  

calculateByReactangles :: (Double -> Maybe Double) -> Double -> Double -> Double -> Int -> Either Error Double
calculateByReactangles f a step ans 0  = Right ans
calculateByReactangles f a step ans parts = do 
  let result = f (a + (step / 2.0))
  case result of
  	Nothing -> Left InputFunctionOrBoundsError
  	Just res -> calculateByReactangles f (a + step) step (ans + res * step) (parts - 1)


partApproxReactangles :: (Double -> Maybe Double) -> Double -> Double -> Double -> Either Error Double
partApproxReactangles f a b d | d == 0.0 = Left NullSizeOfError
                   | otherwise = iterateThroughPartition calculateByReactangles f a b d 0.0 Nothing 1

partApproxTrap :: (Double -> Maybe Double) -> Double -> Double -> Double -> Either Error Double
partApproxTrap f a b d | d == 0.0 = Left NullSizeOfError
                   | otherwise = iterateThroughPartition calculateByReactangles f a b d 0.0 Nothing 1


