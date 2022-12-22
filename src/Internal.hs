module Internal where
import           Expr              (Error (..), eval)
import qualified Expr              as E

data CalculationInput = CalculationInput
  { f     :: (Double -> E.Expr)
  , a     :: Double
  , step  ::Double
  , ans   :: Double
  , parts :: Int
}


max_value = 10^18


getStep :: E.Input -> Int -> Double
getStep input parts = ((E.b input) - (E.a input)) / (fromIntegral parts)

checkValueOverflow :: Double -> Bool
checkValueOverflow  t = (t > max_value) || (t < (-max_value))

trapMethodTerm :: E.Input -> Either Error Double
trapMethodTerm input = do
  let fa = eval $ ( (E.f input) (E.a input))
  let fb = eval $ ( (E.f input) (E.b input))
  case (fa, fb) of
    (Right t1, Right t2) -> if (checkValueOverflow t1 || checkValueOverflow t2)
      then Left SomeIntegralError
      else Right ((t1 + t2) / 2)
    (Left e1, _) -> Left e1
    (_, Left e2) -> Left e2


iterateThroughPartition :: (CalculationInput -> Either Error Double) -> E.Input -> Maybe Double -> Int -> Double -> Either Error E.Output
iterateThroughPartition calculateByXXX input (Just lastIntegration) parts initIntegration = do
  if ((fromIntegral parts) > max_value || lastIntegration > max_value || lastIntegration < (-max_value))
    then Left SomeIntegralError
  else do
   let step = getStep input parts
   let integration = calculateByXXX $ CalculationInput {f = (E.f input), a = (E.a input), step = step
   , ans = (initIntegration * step), parts = parts}
   case integration of
     Left e -> Left e
     Right result -> if (abs (result - lastIntegration) < (E.eps input))
        then Right $ E.Output {E.result = result, E.previousResult = lastIntegration, E.parts = parts}
        else iterateThroughPartition calculateByXXX input (Just result) (parts * 2) initIntegration

iterateThroughPartition calculateByXXX input Nothing parts initIntegration = do
  let step = getStep input parts
  let integration = calculateByXXX $ CalculationInput {f = (E.f input), a = (E.a input), step = step
  , ans = (initIntegration * step), parts = parts}
  case integration of
    Left e -> Left e
    Right result -> iterateThroughPartition calculateByXXX input (Just result) (parts * 2) initIntegration



calculateByReactangles :: CalculationInput -> Either Error Double
calculateByReactangles input | (parts input) == 0 =  Right (ans input)
                             | otherwise = do
  let result = eval $ (f input) ((a input) + (step input) / 2)
  case result of
    Left ex -> Left ex
    Right res -> if (checkValueOverflow res)
      then Left SomeIntegralError
      else calculateByReactangles $ input {a = (a input) + (step input) 
      , ans = (ans input) + res * (step input), parts = (parts input) - 1}


calculateByTrap :: CalculationInput -> Either Error Double
calculateByTrap input | (parts input) == 1 =  Right (ans input)
                      | otherwise =  do
  let result = eval $ (f input) ((a input) + (step input))
  case result of
    Left ex -> Left ex
    Right res -> if (res > max_value || res < (-max_value))
      then Left SomeIntegralError
      else calculateByTrap $ input {a = (a input) + (step input)
      , ans = (ans input) + res * (step input), parts = (parts input) - 1}


calculateBySimpson :: CalculationInput -> Either Error Double
calculateBySimpson input | (parts input) == 0  = Right (ans input)
                         | otherwise = do
  let trap2 = calculateByTrap $ input {step = (step input) / 2, ans = (ans input) / 2, parts = (parts input) * 2}
  let trap1 = calculateByTrap input
  case (trap1, trap2) of
    (Right t1, Right t2) -> if (checkValueOverflow t1 || checkValueOverflow t2)
      then Left SomeIntegralError
      else Right ((4 * t2 - t1) / 3)
    (Left ex1, _) -> Left ex1
    (_, Left ex2) -> Left ex2
