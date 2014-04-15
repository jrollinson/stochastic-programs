module Stochastic
( ShallowExpression(..)
, DeepExpression(..)
, Variable
, Tag
, sample
, dist
, pEval
) where

-- TODO: Add tag checking

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random

type Variable = Int
type Tag = String
type Probability = Double

type Dist a = [(a, Probability)]
certainly x = [(x, 1)]

relative :: [Probability] -> [a] -> Dist a
relative ps xs =
  let s = sum ps
  in map (\(p, x) -> (x, p/s)) $ zip ps xs

flattenDist :: Dist (Dist a) -> Dist a
flattenDist = concat . map (\(dist, p) -> map (\(n,q) -> (n, q * p)) dist)

-- A deep expression can have nested expressions
data DeepExpression = DDataStruct Tag [DeepExpression]
                    | DIndex Tag Int
                    | DIf DeepExpression DeepExpression DeepExpression
                    | DFlip Probability
                    deriving (Eq, Show)

-- A shallow expression only has one level of expressions
data ShallowExpression v = SDataStruct Tag [v]
                            | SIndex v Int
                            | SIf v v v
                            | SFlip Probability
                            deriving (Eq, Show)

-- Defines deep and shallow networks of variables to expressions
type DeepNetwork = Map.Map Variable DeepExpression
type ShallowNetwork var = Map.Map var (ShallowExpression var)

-- True and false shallow expressions
sTrue = SDataStruct "True" []
sFalse = SDataStruct "False" []

-- True and false deep expressions
dTrue = DDataStruct "True" []
dFalse = DDataStruct "False" []

-- A random flip with probability p returns sTrue or sFalse
sFlip :: (RandomGen g) => Probability -> g -> (ShallowExpression v, g)
sFlip p gen = (if f <= p then sTrue else sFalse, gen')
  where (f,gen') = randomR (0, 1) gen

-- Given a shallow network, and a variable, returns a
sample :: (RandomGen g, Ord v) => ShallowNetwork v-> v -> g -> (ShallowNetwork v, g)
sample net x gen = case net Map.! x of

    -- x is a data structure so we are finished
    SDataStruct _ _ -> (net, gen)

    -- x is an indexed value in a data structure.
    -- We need to sample the variable indexed to get value of x.
    SIndex y i ->
      let
        (net', gen') = sample net y gen
      in
        case Map.lookup y net' of
            Just (SDataStruct t l) ->
              let
                z = (l !! i)
                (net'', gen'') = sample net' z gen'
              in
                (Map.insert x (net'' Map.! z) net'', gen'')

            _ -> (Map.insert x sFalse net', gen')

    -- x is a coin flip. We need to sample to pick whether true or not
    SFlip p ->
      let (exp, gen') = sFlip p gen
      in (Map.insert x exp net, gen')

    -- x is an if statement.
    -- We need to sample y.
    -- If y is bound to true, then we will sample z, otherwise w, and set
    -- that value for x.
    SIf y z w ->
      let
        (net', gen') = sample net y gen
        h = if (net' Map.! y == sTrue) then z else w
        (net'', gen'') = sample net' h gen
        hExp = net'' Map.! h
      in
        (Map.insert x hExp net'', gen'')


dist :: (Eq v, Ord v) => ShallowNetwork v -> v -> Dist (ShallowNetwork v)
dist net x = case net Map.! x of

    SDataStruct _ _ -> certainly net

    -- SIndex y i ->
    --   let
    --     nDist = dist net y


    --   in

    SFlip p -> relative [p, 1-p] [Map.insert x sTrue net, Map.insert x sFalse net]

    SIf y z w ->
      let
        -- First we compute the distribution for y
        yDistribution = dist net y

        -- Computes the distribution for a network with given y value.
        yDists n' =
            let
              h = if (n' Map.! y == sTrue) then z else w
              hDist = dist n' h
            in
              map (\(n, p) -> (Map.insert x (n Map.! h) n, p)) hDist

        -- A distribution of distributions of networks
        dists = map (\(n', p) -> (yDists n', p)) yDistribution

      in flattenDist dists


-- Returns whether x uses y in net
-- x uses y if x = y or a variable in the expression for x uses y.
uses :: (Eq v, Ord v) => ShallowNetwork v -> v -> v -> Bool
uses net x y = if x == y then True else case net Map.! x of

    SDataStruct _ n -> any (\z -> uses net z y) n

    SIndex y' i -> uses net y' y

    SFlip _ -> False

    SIf y' z w -> (uses net y' y) || (uses net z y) || (uses net w y)


-- Returns a set of variables used by variables in the given set in given
-- network
usedSet :: (Eq v, Ord v) => ShallowNetwork v -> Set.Set v -> Set.Set v
usedSet net vs =
    let
      -- Variables in the network
      vars = Map.keysSet net

      -- Set of sets of used variables
      setSetUsed = Set.map (\var -> Set.filter (uses net var) vars) vs

       -- Flattens to one set.
    in unions setSetUsed

-- Returns subset of network of variables used by varibales in v
usedNetwork :: (Ord v) => ShallowNetwork v -> Set.Set v -> ShallowNetwork v
usedNetwork net v =
  let s = usedSet net v
  in Map.filterWithKey (\k _ -> Set.member k s) net


-- Unions the two networks, with second values on top
addAssignments :: (Ord v) => ShallowNetwork v -> ShallowNetwork v ->
                             ShallowNetwork v
addAssignments = Map.unionWith (\n m -> m)

-- Set of variables seen by y above x in network net.
seenBy :: (Eq v, Ord v) => ShallowNetwork v -> v -> v -> Set.Set v
seenBy net y x
    | uses net x y = Set.singleton y
    | otherwise =
        let vars = case net Map.! y of
                      SDataStruct _ vs -> vs
                      SIndex z _ -> [z]
                      SFlip _ -> []
                      SIf a b c -> [a, b, c]
        in
          foldr Set.union Set.empty
          $ map (\z -> seenBy net z x)
          $ filter (/=x) vars

-- Set of variables seen by variables in given set in network net.
seenBySet :: (Eq v, Ord v) => ShallowNetwork v -> Set.Set v -> v -> Set.Set v
seenBySet net s x = unions $ Set.map (\y -> seenBy net y x) s

-- Creates a distribution of networks for given
pEval :: (Ord v) => ShallowNetwork v -> v -> Set.Set v -> Dist (ShallowNetwork v)
pEval net x vs = pHelp (usedNetwork net (Set.singleton x)) x vs
    where
      pHelp net x vs = case net Map.! x of

          SDataStruct t v -> certainly (usedNetwork net vs)

          SFlip p -> relative [p, 1-p]
            [Map.insert x sTrue net, Map.insert x sFalse net]

          SIf y z w ->
            let
              yDist = pEval net y (seenBySet net vs y)

              -- Calculates distribution for network in yDist
              mDist m =
                  let
                    n' = addAssignments net m
                    h = if (n' Map.! y == sTrue) then z else w
                    mD = pEval net h $ seenBySet n' vs h

                    extend (m',p) =
                      let e = m' Map.! h
                      in (Map.insert x e $ addAssignments n' m', p)

                  in map extend mD

              dists = map (\(m,p) -> (mDist m, p)) yDist

            in
              flattenDist dists
