module Expr where


-- Тип данных для выражений.

data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Exp Expr
          | Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Sqrt Expr Int
          deriving (Show, Eq)

data Error = DivisionByZero
            | LogOfZero
            | LogOfNegativeNumber
            | SqrtOfNegativeNumber
            | SqrtWithSmallDegree 
            | NullSizeOfError
            | SomeIntegralError
            deriving (Show, Eq)


totalDivEither :: Double -> Double -> Either Error Double
totalDivEither x y | y == 0.0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either Error Double
totalLogEither x | x == 0.0 = Left LogOfZero
                 | x < 0.0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalSqrtEither :: Double -> Int -> Either Error Double
totalSqrtEither x y | x < 0.0 = Left SqrtOfNegativeNumber
                    | y < 2 = Left SqrtWithSmallDegree
                    | otherwise = Right $ x ** (1.0 / (fromIntegral y))


eval :: Expr -> Either Error Double
eval (Val n) = return n
eval (Div x y) = do
  x' <- eval x         -- eval x >>= \x' ->
  y' <- eval y         -- eval y >>= \y' ->
  totalDivEither x' y' -- totalDivEither x' y'
eval (Log x) = do
  x' <- eval x         -- eval x >>= \x' ->
  totalLogEither x'    -- totalLogEither x'
eval (Exp x) = do
  x' <- eval x
  Right $ exp x'
eval (Sum x y) = do
  x' <- eval x
  y' <- eval y
  Right $ (x' + y')
eval (Sub x y) = do
  x' <- eval x
  y' <- eval y
  Right $ (x' - y')
eval (Mul x y) = do
  x' <- eval x
  y' <- eval y
  Right $ (x' * y')
eval (Sqrt x y) = do
  x' <- eval x
  totalSqrtEither x' y