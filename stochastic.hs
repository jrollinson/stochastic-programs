module Stochastic
( ShallowExpression(..)
, DeepExpression(..)
, Variable
, Tag
, sample
, pEval
) where

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

-- A deep expression can have nested expressions
data DeepExpression = DDataStruct Tag [DeepExpression]
                    | DIndex Tag Int
                    | DIf DeepExpression DeepExpression DeepExpression
                    | DFlip Probability
                    deriving (Eq, Show)

-- A shallow expression only has one level of expressions
data ShallowExpression = SDataStruct Tag [Variable]
                       | SIndex Variable Int
                       | SIf Variable Variable Variable
                       | SFlip Probability
                       deriving (Eq, Show)
                       
-- True and false shallow expressions
sTrue = SDataStruct "True" []
sFalse = SDataStruct "False" []

-- True and false deep expressions
dTrue = DDataStruct "True" []
dFalse = DDataStruct "False" []

-- A random flip with probability p returns sTrue or sFalse
sFlip :: (RandomGen g) => Probability -> g -> (ShallowExpression, g)
sFlip p gen = (if f <= p then sTrue else sFalse, gen')
  where (f,gen') = randomR (0, 1) gen

-- Defines deep and shallow networks of variables to expressions
type DeepNetwork = Map.Map Variable DeepExpression
type ShallowNetwork = Map.Map Variable ShallowExpression

-- Given a shallow network, and a variable, returns a 
sample :: (RandomGen g) => ShallowNetwork -> Variable -> g -> (ShallowNetwork, g)
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


dist :: ShallowNetwork -> Variable -> Dist ShallowNetwork
dist net x = case net Map.! x of
    
    SDataStruct _ _ -> certainly net

    -- SIndex y i ->
    --   let 
    --     nDist = dist net y
        

    --   in

    SFlip p -> relative [p, 1-p] [Map.insert x sTrue net, Map.insert x sFalse net]
        
    -- SIF y z w ->
    --   let
    --     nDist' = dist net y


-- Returns whether x uses y in net
-- x uses y if x = y or a variable in the expression for x uses y.
uses :: ShallowNetwork -> Variable -> Variable -> Bool
uses net x y = if x == y then True else case net Map.! x of
    
    SDataStruct _ n -> any (\z -> uses net z y) n
    
    SIndex y' i -> uses net y' y
  
    SFlip _ -> False

    SIf y' z w -> (uses net y' y) || (uses net z y) || (uses net w y)


-- Returns a set of variables used by variables in the given set in given
-- network
usedSet :: ShallowNetwork -> Set.Set Variable -> Set.Set Variable
usedSet net = Set.foldr Set.union Set.empty
            . Set.map (\x -> Set.filter (uses net x) $ Map.keysSet net)

-- Returns subset of network of variables used by varibales in v
usedNetwork :: ShallowNetwork -> Set.Set Variable -> ShallowNetwork
usedNetwork net v = 
  let s = usedSet net v
  in Map.filterWithKey (\k _ -> Set.member k s) net


-- Unions the two networks, with second values on top
addAssigments :: ShallowNetwork -> ShallowNetwork -> ShallowNetwork
addAssigments = Map.unionWith (\n m -> m)

-- Set of variables seen by y above x in network net.
seenBy :: ShallowNetwork -> Variable -> Variable -> Set.Set Variable
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
 
pEval :: ShallowNetwork -> Variable -> Set.Set Variable -> Dist ShallowNetwork
pEval net x vs = pHelp (usedNetwork net (Set.singleton x)) x vs
    where
      pHelp :: ShallowNetwork -> Variable -> Set.Set Variable -> Dist ShallowNetwork
      pHelp net x vs = case net Map.! x of
          
          SDataStruct t v -> certainly (usedNetwork net vs)

          SFlip p -> relative [p, 1-p] 
            [Map.insert x sTrue net, Map.insert x sFalse net]

          -- SIf y z w ->
            
