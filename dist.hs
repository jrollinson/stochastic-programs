import Stochastic

import System.Random
import qualified Data.Map as Map
import qualified Data.Set as Set

expToString (v, exp) = show v ++ " := " ++ show exp

network = Map.fromList [ ("earthquake", SFlip 0.01)
                       , ("burglary", SFlip 0.1)
                       , ("a-when-e-b", SFlip 0.99)
                       , ("a-when-e-nb", SFlip 0.2)
                       , ("a-when-ne-b", SFlip 0.98)
                       , ("a-when-ne-nb", SFlip 0.01)
                       , ("a-when-e", SIf "burglary" "a-when-e-b" "a-when-e-nb")
                       , ("a-when-ne", SIf "burglary" "a-when-ne-b" "a-when-ne-nb")
                       , ("alarm", SIf "earthquake" "a-when-e" "a-when-ne")
                       ]

result =
    let
      d = dist network "alarm"

      netToString n = concatMap (\x -> expToString x ++ "\n") (Map.toList n)

    in
      concatMap (\(n, p) -> "P: " ++ show p ++ "\n" ++ netToString n ++ "\n")
        d


main = do
  putStr $ result
