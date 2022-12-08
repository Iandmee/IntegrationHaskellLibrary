module Integration where
import           Expr              (Error (..), eval)
import qualified Expr              as E
import qualified Internal as I


partApproxReactangles :: E.Input -> Either Error E.Output
partApproxReactangles input | (E.eps input) == 0.0 = Left NullSizeOfError
                   | otherwise = I.iterateThroughPartition I.calculateByReactangles input Nothing 1 0.0


partApproxTrap :: E.Input -> Either Error E.Output
partApproxTrap input | (E.eps input) == 0.0 = Left NullSizeOfError
                    | otherwise = do
                              res <- I.trapMethodTerm input
                              I.iterateThroughPartition I.calculateByTrap input {E.eps = (E.eps input) * 3} Nothing 1 res


partApproxSimpson :: E.Input -> Either Error E.Output
partApproxSimpson input | (E.eps input) == 0.0 = Left NullSizeOfError
                        | otherwise = do
                              res <- I.trapMethodTerm input
                              I.iterateThroughPartition I.calculateByTrap input {E.eps = (E.eps input) * 3} Nothing 1 res

