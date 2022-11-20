module Main (main) where
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Integration
import Expr
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector
f1 :: Double -> Expr
f1 x = Bin Mul (Val x) (Val x)

f2 :: Double -> Expr
f2 x = Bin Div (Val 1) (Val x)


myCircle :: Diagram B
myCircle = circle 1

example :: Diagram B
example = fromOffsets [unitX, unitY, 2 *^ unit_X, unit_Y] # centerXY

main :: IO()
main = mainWith example