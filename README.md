# IntegrationHaskellLibrary

# Usage 

## Expressions

```Haskell
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

```

For calculation integral of your function, it must be presented as **Expr** type.

#### Examples:

- $f(x) = x + \log{0.0}$
 ```Haskell 
f :: Double -> Expr
f x = Bin Sum (Un Log (Val 0.0)) (Val x)
```

- $f(x) = \sqrt{x}$  
```Haskell
f :: Double -> Expr
f x = Sqrt (Val x) 2
```

-  $f(x) = e^x$ 
```Haskell
f :: Double -> Expr
f x = Un Exp (Val x)
```


## Input
To calculate the integral you have to create the following **Input**:
```Haskell
data Input = Input
  { f   :: (Double -> Expr)
  , a   :: Double
  , b   :: Double
  , eps :: Double
}
```
`f` - your function presented as **Expr** type.
`a` - left bound 
b - right bound
`eps` - maximum size of the error
Must be satisfied:
- $a \leq b$ 
- $eps > 0$

## Methods
1. `partApproxReactangles`  - calculating your integral using approximation by rectangles.
2.  `partApproxTrap` - calculating your integral using approximation by rectangles.
3.  `partApproxSimpson` - calculating your integral using the Simson method.
#### Example:
```Haskell
f1 :: Double -> Expr
f1 x = Bin Mul (Val x) (Val x)

main :: IO()
main = do
  let input = Input {f = f1, a = 1.0, b = 2.0, eps = 0.001}
  let r = I.partApproxReactangles input
  putStrLn $ show $ r
  let r = I.partApproxTrap input
  putStrLn $ show $ r
  let r = I.partApproxSimpson input
  putStrLn $ show $ r
```
#### Output:
```Haskell
Right (Output {result = 2.3330078125, previousResult = 2.33203125, parts = 16})
Right (Output {result = 2.333984375, previousResult = 2.3359375, parts = 16})
Right (Output {result = 2.333984375, previousResult = 2.3359375, parts = 16})
```
`parts` - the minimum number of parts into which a segment should be divided to achieve inputted error size in each method.
## Diagrams

To create a method comparison diagram, you can change **inputFunction** in `comparison/Main.hs`. Then write in the console:
```Bash
stack run comparison
```
It will produce `comparison.jpg` with the required diagram.
<br>
<br>
 <img src="https://github.com/Iandmee/IntegrationHaskellLibrary/blob/main/img/comparison1.png" alt="alt text" title="image Title" width="450"/>

(in the example red line behind the blue one)
<br>
<br>

$Ox$ - means, that error = $e^{-x}$
<br>
$Oy$ - value of the integral

## Architecture
The main logic is hidden in the `Internal.hs`, where implemented a logic of 3 methods is.
Library interface, which the user is supposed to use located in `Integration.hs`.
Implemented tests consist of  `Unit tests` and `Property based` ones.
The logic of diagrams production is located in `comparison/Main.hs` 

## Libraries

For *Property based* tests I decided to use `Hedgehog` library for creating generators, ranges and so on.  
For *Diagrams* I decided to use  `Matplotlib` library which uses python for generating pictures.
