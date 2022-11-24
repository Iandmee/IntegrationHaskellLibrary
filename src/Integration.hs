module Integration where
import           Expr              (Error (..), eval)
import qualified Expr              as E
import qualified Internal as I


partApproxReactangles :: E.Input -> Either Error E.Output
partApproxReactangles input | (E.eps input) == 0.0 = Left NullSizeOfError
                   | otherwise = I.iterateThroughPartition I.calculateByReactangles input 0.0 Nothing 1


partApproxTrap :: E.Input -> Either Error E.Output
partApproxTrap input | (E.eps input) == 0.0 = Left NullSizeOfError
                       | otherwise = do
                           case initIntegration of
                             Right res -> I.iterateThroughPartition I.calculateByTrap input res Nothing 1
                             Left ex -> Left ex
                   where
                     initIntegration = I.trapMethodTerm input


partApproxSimpson :: E.Input -> Either Error E.Output
partApproxSimpson input | (E.eps input) == 0.0 = Left NullSizeOfError
                          | otherwise = do
                              case initIntegration of
                                Right res -> I.iterateThroughPartition I.calculateBySimpson input res Nothing 1
                                Left ex -> Left ex
                      where
                        initIntegration = I.trapMethodTerm input

