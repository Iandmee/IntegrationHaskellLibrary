module Integration where
import Control.Exception(catch, try, evaluate, SomeException(..))
import Expr

data Output = Output
  { result :: Double
  , previousResult :: Double
  , parts :: Int
  }
  deriving (Show, Eq)

max_value = 10^9

trapMethodTerm :: (Double -> Expr) -> Double -> Double -> Either Error Double
trapMethodTerm f a b = do 
  let fa = eval $ (f a)
  let fb = eval $ (f b)
  case (fa, fb) of 
    (Right t1, Right t2) -> if (t1 > max_value || t1 < (-max_value) || t2 > max_value || t2 < (-max_value))
      then Left SomeIntegralError
      else Right ((t1 + t2) / 2)
    (Left e1, _) -> Left e1
    (_, Left e2) -> Left e2


iterateThroughPartition :: ((Double -> Expr) -> Double -> Double -> Double -> Int -> Either Error Double) -> (Double -> Expr) -> Double -> Double -> Double -> Double -> Maybe Double -> Int -> Either Error Output
iterateThroughPartition calculateByXXX f a b d initIntegration (Just lastIntegration) parts = do 
  if ((fromIntegral parts) > max_value || lastIntegration > max_value || lastIntegration < (-max_value))
    then Left SomeIntegralError
  else do
   let step = (b - a) / (fromIntegral parts)
   let integration = calculateByXXX f a step (initIntegration * step) parts
   case integration of 
     Left e -> Left e
     Right result -> if (abs (result - lastIntegration) < d) 
        then Right $ Output {result = result, previousResult = lastIntegration, parts = parts} 
        else iterateThroughPartition calculateByXXX f a b d initIntegration (Just result) (parts + 1)

iterateThroughPartition calculateByXXX f a b d initIntegration Nothing parts = do
  let step = (b - a) / (fromIntegral parts)
  let integration = calculateByXXX f a step (initIntegration * step) parts
  case integration of 
    Left e -> Left e
    Right result -> iterateThroughPartition calculateByXXX f a b d initIntegration (Just result) (parts + 1)

  

calculateByReactangles :: (Double -> Expr) -> Double -> Double -> Double -> Int -> Either Error Double
calculateByReactangles f a step ans 0 =  Right ans
calculateByReactangles f a step ans parts = do 
  let result = eval $ f (a + (step / 2))
  case result of
    Left ex -> Left ex
    Right res -> if (res > max_value || res < (-max_value)) 
      then Left SomeIntegralError 
      else calculateByReactangles f (a + step) step (ans + res * step) (parts - 1)


calculateByTrap :: (Double -> Expr) -> Double -> Double -> Double -> Int -> Either Error Double 
calculateByTrap f a step ans 1 =  Right ans
calculateByTrap f a step ans parts = do 
  let result = eval $ f (a + step)
  case result of
    Left ex -> Left ex
    Right res -> if (res > max_value || res < (-max_value))
      then Left SomeIntegralError
      else calculateByTrap f (a + step) step (ans + res * step) (parts - 1)


calculateBySimpson :: (Double -> Expr) -> Double -> Double -> Double -> Int -> Either Error Double
calculateBySimpson f a step ans 0  = Right ans
calculateBySimpson f a step ans parts = do 
  let trap2 = calculateByTrap f a (step / 2) (ans / 2) (parts * 2)
  let trap1 = calculateByTrap f a step ans parts
  case (trap1, trap2) of 
    (Right t1, Right t2) -> if (t1 > max_value || t1 < (-max_value) || t2 > max_value || t2 < (-max_value))
      then Left SomeIntegralError
      else Right ((4 * t2 - t1) / 3)
    (Left ex1, _) -> Left ex1
    (_, Left ex2) -> Left ex2


partApproxReactangles :: (Double -> Expr) -> Double -> Double -> Double -> Either Error Output
partApproxReactangles f a b d | d == 0.0 = Left NullSizeOfError
                   | otherwise = iterateThroughPartition calculateByReactangles f a b d 0.0 Nothing 1


partApproxTrap :: (Double -> Expr) -> Double -> Double -> Double -> Either Error Output
partApproxTrap f a b d | d == 0.0 = Left NullSizeOfError
                       | otherwise = do
                           case initIntegration of
                             Right res -> iterateThroughPartition calculateByTrap f a b d res Nothing 1
                             Left ex -> Left ex
                   where
                     initIntegration = trapMethodTerm f a b


partApproxSimpson :: (Double -> Expr) -> Double -> Double -> Double -> Either Error Output
partApproxSimpson f a b d | d == 0.0 = Left NullSizeOfError
                          | otherwise = do
                              case initIntegration of
                                Right res -> iterateThroughPartition calculateBySimpson f a b d res Nothing 1
                                Left ex -> Left ex
                      where
                        initIntegration = trapMethodTerm f a b
              


