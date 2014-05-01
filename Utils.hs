-- |Utilities used by Stochastic Function modules
module Utils
( unions
) where

import qualified Data.Set as Set
import qualified Data.Map as Map

-- |Unions a set of sets
unions :: (Ord a) => Set.Set (Set.Set a) -> Set.Set a
unions = Set.foldl Set.union Set.empty

-- |Turns network into a string with newlines for printing
pNet net = Map.foldrWithKey (\k a s -> s ++ show k ++ " : " ++ show a ++ "\n") "" net
