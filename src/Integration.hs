module Integration where
import Control.Exception(catch, try, evaluate, SomeException(..))
import Expr
data Error = InputFunctionOrBoundsError
           | NullSizeOfError
           deriving (Show, Eq)

trapMethodTerm :: (Double -> Double) -> Double -> Double -> Double
trapMethodTerm f a b = ((f a) + (f b)) / 2


iterateThroughPartition :: ((Double -> Double) -> Double -> Double -> Double -> Int -> IO (Either Error Double)) -> (Double -> Double) -> Double -> Double -> Double -> Double -> Maybe Double -> Int -> IO (Either Error Double)
iterateThroughPartition calculateByXXX f a b d initIntegration lastIntegration parts = do 
  let step = (b - a) / (fromIntegral parts)
  integration <- calculateByXXX f a step (initIntegration * step) parts
  case integration of 
    Left e -> return $ Left e
    Right result -> case lastIntegration of 
      Nothing -> iterateThroughPartition calculateByXXX f a b d initIntegration (Just result) (parts + 1)
      Just lastResult -> if (abs (result - lastResult) < d) then return $ Right result else iterateThroughPartition calculateByXXX f a b d initIntegration (Just result) (parts + 1)
  

calculateByReactangles :: (Double -> Double) -> Double -> Double -> Double -> Int -> IO (Either Error Double)
calculateByReactangles f a step ans 0 = return $ Right ans
calculateByReactangles f a step ans parts = do 
  result <- try (evaluate $ f (a + (step / 2))) :: IO (Either SomeException  Double)
  case result of
  	Left ex -> return $ Left InputFunctionOrBoundsError
  	Right res -> calculateByReactangles f (a + step) step (ans + res * step) (parts - 1)


calculateByTrap :: (Double -> Double) -> Double -> Double -> Double -> Int -> IO (Either Error Double)  
calculateByTrap f a step ans 1  = return $ Right ans
calculateByTrap f a step ans parts = do 
  result <- try (evaluate $ f (a + step)) :: IO (Either SomeException  Double)
  case result of
    Left ex -> return $ Left InputFunctionOrBoundsError
    Right res -> calculateByTrap f (a + step) step (ans + res * step) (parts - 1)


calculateBySimpson :: (Double -> Double) -> Double -> Double -> Double -> Int -> IO (Either Error Double)
calculateBySimpson f a step ans 0  = return $ Right ans
calculateBySimpson f a step ans parts = do 
  trap2 <- calculateByTrap f a (step / 2) (ans / 2) (parts * 2)
  trap1 <- calculateByTrap f a step ans parts
  case (trap1, trap2) of 
    (Right t1, Right t2) -> return $ Right ((4 * t2 - t1) / 3)
    otherwise -> return $ Left InputFunctionOrBoundsError 


partApproxReactangles :: (Double -> Double) -> Double -> Double -> Double -> IO (Either Error Double)
partApproxReactangles f a b d | d == 0.0 = return $ Left NullSizeOfError
                   | otherwise = iterateThroughPartition calculateByReactangles f a b (d / 10) 0.0 Nothing 1


partApproxTrap :: (Double -> Double) -> Double -> Double -> Double -> IO (Either Error Double)
partApproxTrap f a b d | d == 0.0 = return $ Left NullSizeOfError
                       | otherwise = do
                           r <- initIntegration
                           case r of
                             Right res -> iterateThroughPartition calculateByTrap f a b (d / 10) res Nothing 1
                             Left ex -> return $ Left InputFunctionOrBoundsError
                   where
                     initIntegration = try (evaluate (trapMethodTerm f a b)) :: IO (Either SomeException Double)


partApproxSimpson :: (Double -> Double) -> Double -> Double -> Double -> IO (Either Error Double)
partApproxSimpson f a b d | d == 0.0 = return $ Left NullSizeOfError
                          | otherwise = do
                              r <- initIntegration
                              case r of
                                Right res -> iterateThroughPartition calculateBySimpson f a b (d  / 10) res Nothing 1
                                Left ex -> return $ Left InputFunctionOrBoundsError
                      where
                        initIntegration = try (evaluate (trapMethodTerm f a b)) :: IO (Either SomeException Double) 
              


