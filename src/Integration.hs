module Integration where
import           Control.Exception (SomeException (..), catch, evaluate, try)
import           Expr              (Error (..), eval)
import qualified Expr              as E

data CalculationInput = CalculationInput
  { f     :: (Double -> E.Expr)
  , a     :: Double
  , step  ::Double
  , ans   :: Double
  , parts :: Int
}


max_value = 10^9

trapMethodTerm :: E.Input -> Either Error Double
trapMethodTerm input = do
  let fa = eval $ ( (E.f input) (E.a input))
  let fb = eval $ ( (E.f input) (E.b input))
  case (fa, fb) of
    (Right t1, Right t2) -> if (t1 > max_value || t1 < (-max_value) || t2 > max_value || t2 < (-max_value))
      then Left SomeIntegralError
      else Right ((t1 + t2) / 2)
    (Left e1, _) -> Left e1
    (_, Left e2) -> Left e2


iterateThroughPartition :: (CalculationInput -> Either Error Double) -> E.Input -> Double -> Maybe Double -> Int -> Either Error E.Output
iterateThroughPartition calculateByXXX input initIntegration (Just lastIntegration) parts = do
  if ((fromIntegral parts) > max_value || lastIntegration > max_value || lastIntegration < (-max_value))
    then Left SomeIntegralError
  else do
   let step = ( (E.b input) - (E.a input)) / (fromIntegral parts)
   let integration = calculateByXXX $ CalculationInput {f = (E.f input), a = (E.a input), step = step
   , ans = (initIntegration * step), parts = parts}
   case integration of
     Left e -> Left e
     Right result -> if (abs (result - lastIntegration) < (E.eps input))
        then Right $ E.Output {E.result = result, E.previousResult = lastIntegration, E.parts = parts}
        else iterateThroughPartition calculateByXXX input initIntegration (Just result) (parts + 1)

iterateThroughPartition calculateByXXX input initIntegration Nothing parts = do
  let step = ( (E.b input) - (E.a input)) / (fromIntegral parts)
  let integration = calculateByXXX $ CalculationInput {f = (E.f input), a = (E.a input), step = step
  , ans = (initIntegration * step), parts = parts}
  case integration of
    Left e -> Left e
    Right result -> iterateThroughPartition calculateByXXX input initIntegration (Just result) (parts + 1)



calculateByReactangles :: CalculationInput -> Either Error Double
calculateByReactangles input | (parts input) == 0 =  Right (ans input)
                             | otherwise = do
  let result = eval $ (f input) ( (a input) + ((step input) / 2))
  case result of
    Left ex -> Left ex
    Right res -> if (res > max_value || res < (-max_value))
      then Left SomeIntegralError
      else calculateByReactangles $ CalculationInput {f = (f input), a = ( (a input) + (step input) )
      , step = (step input), ans = ( (ans input) + res * (step input)), parts = ( (parts input) - 1)}


calculateByTrap :: CalculationInput -> Either Error Double
calculateByTrap input | (parts input) == 1 =  Right (ans input)
                      | otherwise =  do
  let result = eval $ (f input) ((a input) + (step input))
  case result of
    Left ex -> Left ex
    Right res -> if (res > max_value || res < (-max_value))
      then Left SomeIntegralError
      else calculateByTrap $ CalculationInput {f = (f input), a = ( (a input) + (step input))
      , step = (step input), ans = ((ans input) + res * (step input)), parts = ((parts input) - 1)}


calculateBySimpson :: CalculationInput -> Either Error Double
calculateBySimpson input | (parts input) == 0  = Right (ans input)
                         | otherwise = do
  let trap2 = calculateByTrap $ CalculationInput {f = (f input), a = (a input)
  , step = ( (step input) / 2), ans = ((ans input) / 2), parts = ( (parts input) * 2)}
  let trap1 = calculateByTrap input
  case (trap1, trap2) of
    (Right t1, Right t2) -> if (t1 > max_value || t1 < (-max_value) || t2 > max_value || t2 < (-max_value))
      then Left SomeIntegralError
      else Right ((4 * t2 - t1) / 3)
    (Left ex1, _) -> Left ex1
    (_, Left ex2) -> Left ex2


partApproxReactangles :: E.Input -> Either Error E.Output
partApproxReactangles input | (E.eps input) == 0.0 = Left NullSizeOfError
                   | otherwise = iterateThroughPartition calculateByReactangles input 0.0 Nothing 1


partApproxTrap :: E.Input -> Either Error E.Output
partApproxTrap input | (E.eps input) == 0.0 = Left NullSizeOfError
                       | otherwise = do
                           case initIntegration of
                             Right res -> iterateThroughPartition calculateByTrap input res Nothing 1
                             Left ex -> Left ex
                   where
                     initIntegration = trapMethodTerm input


partApproxSimpson :: E.Input -> Either Error E.Output
partApproxSimpson input | (E.eps input) == 0.0 = Left NullSizeOfError
                          | otherwise = do
                              case initIntegration of
                                Right res -> iterateThroughPartition calculateBySimpson input res Nothing 1
                                Left ex -> Left ex
                      where
                        initIntegration = trapMethodTerm input

