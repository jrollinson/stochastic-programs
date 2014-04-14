import Stochastic

import System.Random
import qualified Data.Map as Map
import qualified Data.Set as Set

expToString (v, exp) = show v ++ " := " ++ show exp

network = Map.fromList [ (0, SFlip 0.01) -- earthquake
                       , (1, SFlip 0.1)  -- burglary
                       , (2, SFlip 0.99) -- a-when-e-b
                       , (3, SFlip 0.2)  -- a-when-e-nb
                       , (4, SFlip 0.98) -- a-when-ne-b
                       , (5, SFlip 0.01) -- a-when-ne-nb
                       , (6, SIf 1 2 3)  -- a-when-e
                       , (7, SIf 1 4 5)  -- a-when-ne
                       , (8, SIf 0 6 7)  -- alarm
                       ]

result = 
    let 
      dist = pEval network 8 (Set.singleton 8)

      netToString n = concatMap (\x -> expToString x ++ "\n") (Map.toList n)

    in 
      concatMap (\(n, p) -> "P: " ++ show p ++ "\n" ++ netToString n ++ "\n")
        dist


main = do
  putStr $ result
