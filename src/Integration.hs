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
  let result = f (a + (step / 2))
  case result of
  	Nothing -> Left InputFunctionOrBoundsError
  	Just res -> calculateByReactangles f (a + step) step (ans + res * step) (parts - 1)

calculateByTrap :: (Double -> Maybe Double) -> Double -> Double -> Double -> Int -> Either Error Double
calculateByTrap f a step ans 1  = Right ans
calculateByTrap f a step ans parts = do 
  let result = f (a + step)
  case result of
    Nothing -> Left InputFunctionOrBoundsError
    Just res -> calculateByTrap f (a + step) step (ans + res * step) (parts - 1)

calculateBySimpson :: (Double -> Maybe Double) -> Double -> Double -> Double -> Int -> Either Error Double
calculateBySimpson f a step ans 0  = Right ans
calculateBySimpson f a step ans parts = do 
  let trap2 = calculateByTrap f a (step / 2) (ans / 2) (parts * 2)
  let trap1 = calculateByTrap f a step ans parts
  case (trap1, trap2) of 
    (Right t1, Right t2) -> Right ((4 * t2 - t1) / 3)
    otherwise -> Left InputFunctionOrBoundsError 

partApproxReactangles :: (Double -> Maybe Double) -> Double -> Double -> Double -> Either Error Double
partApproxReactangles f a b d | d == 0.0 = Left NullSizeOfError
                   | otherwise = iterateThroughPartition calculateByReactangles f a b d 0.0 Nothing 1

partApproxTrap :: (Double -> Maybe Double) -> Double -> Double -> Double -> Either Error Double
partApproxTrap f a b d | d == 0.0 = Left NullSizeOfError
                       | f a == Nothing = Left InputFunctionOrBoundsError
                       | f b == Nothing = Left InputFunctionOrBoundsError
                       | otherwise = iterateThroughPartition calculateByTrap f a b d initIntegration Nothing 1
                   where
                     initIntegration = case ((f a), (f b)) of 
                       (Just y1, Just y2) -> (y1 + y2) / 2

partApproxSimpson :: (Double -> Maybe Double) -> Double -> Double -> Double -> Either Error Double
partApproxSimpson f a b d | d == 0.0 = Left NullSizeOfError
                          | f a == Nothing = Left InputFunctionOrBoundsError
                          | f b == Nothing = Left InputFunctionOrBoundsError
                          | otherwise = iterateThroughPartition calculateBySimpson f a b d initIntegration Nothing 1
                      where
                        initIntegration = case ((f a), (f b)) of 
                          (Just y1, Just y2) -> (y1 + y2) / 2


