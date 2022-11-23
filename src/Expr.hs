module Expr where


-- Тип данных для выражений.

data Expr = Val Double 
          | Bin BinOp Expr Expr
          | Un UnOp Expr
          | Sqrt Expr Int
          deriving (Show, Eq)

data BinOp = Div
          | Sum
          | Sub
          | Mul
          deriving (Show, Eq)

data UnOp = Log
          | Exp
          deriving (Show, Eq)



data Error = DivisionByZero
            | LogOfZero
            | LogOfNegativeNumber
            | SqrtOfNegativeNumber
            | SqrtWithSmallDegree 
            | NullSizeOfError
            | SomeIntegralError
            deriving (Show, Eq)

data Output = Output
  { result :: Double
  , previousResult :: Double
  , parts :: Int
  }
  deriving (Show, Eq)

data Input = Input
  { f :: (Double -> Expr)
  , a :: Double
  , b :: Double
  , eps :: Double
}

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
eval (Bin Div x y) = do
  x' <- eval x         -- eval x >>= \x' ->
  y' <- eval y         -- eval y >>= \y' ->
  totalDivEither x' y' -- totalDivEither x' y'
eval (Un Log x) = do
  x' <- eval x         -- eval x >>= \x' ->
  totalLogEither x'    -- totalLogEither x'
eval (Un Exp x) = do
  x' <- eval x
  Right $ exp x'
eval (Bin Sum x y) = do
  x' <- eval x
  y' <- eval y
  Right $ (x' + y')
eval (Bin Sub x y) = do
  x' <- eval x
  y' <- eval y
  Right $ (x' - y')
eval (Bin Mul x y) = do
  x' <- eval x
  y' <- eval y
  Right $ (x' * y')
eval (Sqrt x y) = do
  x' <- eval x
  totalSqrtEither x' y