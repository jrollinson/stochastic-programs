module Distribution
( Dist
, Probability
, certainly
, relative
, flattenDist
, dMap
, combineDist
) where

import qualified Data.List as List

type Probability = Double

type Dist a = [(a, Probability)]


-- Returns a singleton distribution
certainly :: a -> Dist a
certainly x = [(x, 1)]

-- Takes a list of frequencies and values and creates a Dist
relative :: [(a, Probability)] -> Dist a
relative l =
  let s = sum $ map (\(_,p) -> p) l
  in map (\(x,p) -> (x, p/s)) l

-- Flattens a distribution
flattenDist :: Dist (Dist a) -> Dist a
flattenDist = concat . map (\(dist, p) -> map (\(n,q) -> (n, q * p)) dist)

-- Maps values in a distribution to other values (NO NORMALIZING)
dMap :: (a -> b) -> Dist a -> Dist b
dMap f d = map (\(x,p) -> (f x, p)) d

-- Combines the same items in a distribution
combineDist :: (Eq a, Ord a, Show a) => Dist a -> Dist a
combineDist d =
  let
    sortedD = List.sortBy (\(n,p) (m,q) -> compare n m) d
    groupedD = List.groupBy (\(n,p) (m,q) -> n == m) sortedD

    toNetP xs =
      let (n,_) = head xs
      in (n, sum $ map (\(_, p) -> p) xs)

  in map toNetP groupedD
