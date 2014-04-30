module Stochastic
( ShallowExpression(..)
, DeepExpression(..)
, Variable
, Tag
, sample
, dist
, dist'
, pEval
, deepToShallow
) where

-- TODO: Add tag checking

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random

import Distribution

type Variable = Int
type Tag = String

-- Type Definitions
-------------------

type DeepExpVar v = Either (DeepExpression v) v

-- A deep expression can have nested expressions
data DeepExpression v = DDataStruct Tag [(DeepExpVar v)]
                      | DIndex (DeepExpVar v) Int
                      | DIf (DeepExpVar v) (DeepExpVar v) (DeepExpVar v)
                      | DFlip Probability
                      deriving (Eq, Ord, Show)

-- A shallow expression only has one level of expressions
data ShallowExpression v = SDataStruct Tag [v]
                            | SIndex v Int
                            | SIf v v v
                            | SFlip Probability
                            deriving (Eq, Ord, Show)

-- Defines deep and shallow networks of variables to expressions
type Network t var = Map.Map var (t var)
type DeepNetwork var = Network DeepExpression var
type ShallowNetwork var = Network ShallowExpression var


-- Helpful values
-----------------

-- True and false shallow expressions
sTrue = SDataStruct "True" []
sFalse = SDataStruct "False" []

-- True and false deep expressions
dTrue = DDataStruct "True" []
dFalse = DDataStruct "False" []


-- Functions
------------

-- Transforms a deep network of string variables into a shallow network of
-- strings
deepToShallow :: DeepNetwork String -> ShallowNetwork String
deepToShallow = Map.fold Map.union Map.empty
              . Map.mapWithKey (\k v -> toShallow k v)
  where
    -- toShallow that takes in DeepExpVar instead of DeepExpression, and also
    -- returns the variable that the network is for
    toShallowVar :: String -> (DeepExpVar String)
                 -> (String, Map.Map String (ShallowExpression String))
    toShallowVar v (Left exp) = (v, toShallow v exp)
    toShallowVar _ (Right var) = (var, Map.empty)


    -- Turns an expression into a variable and shallow network for that var
    toShallow :: String
              -> DeepExpression String
              -> ShallowNetwork String

    toShallow v (DDataStruct tag expVars) =
      let
        valToExp (x:xs) n =
          let
            (v', m) = toShallowVar (v ++ "-" ++ show n) x
            (vs, mainM) = valToExp xs (n+1)
          in
            (v':vs, Map.union m mainM)

        (vs, m) = valToExp expVars 0
      in
        addExp m v (SDataStruct tag vs)


    toShallow v (DIndex expVar i) =
      let (v', m) = toShallowVar (v ++ "-inv") expVar
      in addExp m v (SIndex v' i)

    toShallow v (DIf expVarIf expVarThen expVarElse) =
      let
        (ifV, ifM) = toShallowVar (v ++ "-cond") expVarIf
        (thenV, thenM) = toShallowVar (v ++ "-w-" ++ ifV) expVarThen
        (elseV, elseM) = toShallowVar (v ++ "-w-n-" ++ ifV) expVarElse
        m = Map.unions [ifM, thenM, elseM]

        shallowExp = SIf ifV thenV elseV
      in
        addExp m v shallowExp

    toShallow v (DFlip p) = Map.singleton v (SFlip p)


-- Turns network into a string with newlines for printing
pNet net = Map.foldrWithKey (\k a s -> s ++ show k ++ " : " ++ show a ++ "\n") "" net

unions :: (Ord a) => Set.Set (Set.Set a) -> Set.Set a
unions = Set.foldl Set.union Set.empty

-- Safely gets expression out of a network (never causes exception)
safeGetExp :: (Ord var) => Network exp var -> var -> Maybe (exp var)
safeGetExp net x = Map.lookup x net

-- gets expression out of a network
getExp :: (Ord var) => Network exp var -> var -> exp var
getExp net x = net Map.! x

-- adds expression to a network
addExp :: (Ord var) => Network exp var -> var ->  exp var -> Network exp var
addExp net x exp = Map.insert x exp net

-- A random flip with probability p returns sTrue or sFalse
sFlip :: (RandomGen g) => Probability -> g -> (ShallowExpression v, g)
sFlip p gen = (if f <= p then sTrue else sFalse, gen')
  where (f,gen') = randomR (0, 1) gen


-- | Returns the expression variables of a shallow expression
expressionVars :: ShallowExpression v -> [v]
expressionVars (SDataStruct _ vars) = vars
expressionVars (SIndex var _) = [var]
expressionVars (SIf ifV thenV elseV) = [ifV, thenV, elseV]
expressionVars (SFlip _) = []

-- Given a shallow network, and a variable, returns a
sample :: (RandomGen g, Ord v) => ShallowNetwork v-> v -> g -> (ShallowNetwork v, g)
sample net x gen = case getExp net x of

    -- x is a data structure so we are finished
    SDataStruct _ _ -> (net, gen)

    -- x is an indexed value in a data structure.
    -- We need to sample the variable indexed to get value of x.
    SIndex y i ->
      let
        (net', gen') = sample net y gen
      in
        case safeGetExp net' y of
            Just (SDataStruct t l) ->
              let
                z = (l !! i)
                (net'', gen'') = sample net' z gen'
              in
                (addExp net'' x (getExp net'' z), gen'')

            _ -> (addExp net' x sFalse, gen')

    -- x is a coin flip. We need to sample to pick whether true or not
    SFlip p ->
      let (exp, gen') = sFlip p gen
      in (addExp net x exp, gen')

    -- x is an if statement.
    -- We need to sample y.
    -- If y is bound to true, then we will sample z, otherwise w, and set
    -- that value for x.
    SIf y z w ->
      let
        (net', gen') = sample net y gen
        h = if (getExp net' y == sTrue) then z else w
        (net'', gen'') = sample net' h gen
        hExp = getExp net'' h
      in
        (addExp net'' x hExp, gen'')


dist :: (Eq v, Ord v) => ShallowNetwork v -> v -> Dist (ShallowNetwork v)
dist net x = case getExp net x of

    SDataStruct _ _ -> certainly net

    -- SIndex y i ->
    --   let
    --     nDist = dist net y


    --   in

    SFlip p -> relative [ (addExp net x sTrue, p)
                        , (addExp net x sFalse, 1-p)
                        ]

    SIf y z w ->
      let
        -- First we compute the distribution for y
        yDistribution = dist net y

        -- Computes the distribution for a network with given y value.
        yDists n' =
            let
              h = if (getExp n' y == sTrue) then z else w
              hDist = dist n' h
            in
              dMap (\n -> addExp n x (getExp n h)) hDist

        -- A distribution of distributions of networks
        dists = dMap yDists yDistribution

      in flattenDist dists


-- Uses dist to return a distribution over set V
dist' :: (Show v, Ord v) => ShallowNetwork v -> v -> Set.Set v -> Dist (ShallowNetwork v)
dist' net x vs =
    let
      d = dist net x
      shrunkD = dMap (\n -> usedNetwork n vs) d
    in
      combineDist shrunkD

-- Returns whether x uses y in net
-- x uses y if x = y or a variable in the expression for x uses y.
uses :: (Eq v, Ord v) => ShallowNetwork v -> v -> v -> Bool
uses net x y = if x == y
               then True
               else any (\z -> uses net z y) $ expressionVars $ getExp net x

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
addAssignments :: (Ord v) => Network a v -> Network a v ->
                             Network a v
addAssignments = Map.unionWith (\n m -> m)

-- Set of variables seen by y above x in network net.
seenBy :: (Eq v, Ord v) => ShallowNetwork v -> v -> v -> Set.Set v
seenBy net y x
    | uses net x y = Set.singleton y
    | otherwise =
        -- Variables in y's expressions
        let vars = expressionVars $ getExp net y
        in
          -- Unions the set of varibles seen by each v in vars that is not x.
          Set.unions
          $ map (\z -> seenBy net z x)
          $ filter (/=x) vars

-- Set of variables seen by variables in given set in network net.
seenBySet :: (Eq v, Ord v) => ShallowNetwork v -> Set.Set v -> v -> Set.Set v
seenBySet net vs x = unions $ Set.map (\y -> seenBy net y x) vs

-- Creates a distribution of networks for given
pEval :: (Ord v) => ShallowNetwork v -> v -> Set.Set v -> Dist (ShallowNetwork v)
pEval net x vs = pHelp (usedNetwork net (Set.singleton x)) x vs'
    where
      vs' = Set.insert x vs

      pHelp net x vs = case getExp net x of

          SDataStruct t v -> certainly (usedNetwork net vs)

          SFlip p -> relative [ (Map.singleton x sTrue, p)
                              , (Map.singleton x sFalse, 1-p)]

          SIf y z w ->
            let
              -- TODO: This looks scary!
              yDist = pEval net y (seenBySet net vs y)

              -- Calculates distribution for network in yDist
              mDist m =
                  let
                    n' = addAssignments net m
                    h = if (getExp n' y == sTrue) then z else w
                    -- DISAGREES WITH PAPER ON n' in pEval!!!!
                    mD = pEval n' h (seenBySet n' vs h)

                    extend m' =
                      let
                        e = getExp m' h
                        fullNet = addExp (addAssignments n' m') x e
                      in
                        usedNetwork fullNet vs

                  in dMap extend mD

              dists = dMap mDist yDist

            in
              flattenDist dists
