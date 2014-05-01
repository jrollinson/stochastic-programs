-- |Functions for sampling networks
module Sample
( sample
) where

import System.Random
import Expression
import ShallowExpression
import Distribution

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

-- |A random flip. Returns True with probability p, otherwise False
sFlip :: (RandomGen g) => Probability -> g -> (ShallowExpression v, g)
sFlip p gen = (if f <= p then trueExp else falseExp, gen')
  where (f,gen') = randomR (0, 1) gen

-- |Given a shallow network, and a variable, returns a network with x set
-- randomly by the given function
sample :: (RandomGen g, Ord v, Enum v) => ShallowNetwork v -> v -> g
       -> (ShallowNetwork v, g)
sample net x gen = case getExp net x of

    -- x is a data structure so we are finished
    SDataStruct _ _ -> (net, gen)

    -- x is an indexed value in a data structure.
    -- We need to sample the variable indexed to get value of x.
    SIndex y i ->
      let
        (net', gen') = sample net y gen
      in
        case getExp net' y of
            SDataStruct t l ->
              let
                z = (l !! i)
                (net'', gen'') = sample net' z gen'
              in
                (addExp net'' x (getExp net'' z), gen'')

            _ -> (addExp net' x falseExp, gen')

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
        h = if (getExp net' y == trueExp) then z else w
        (net'', gen'') = sample net' h gen
        hExp = getExp net'' h
      in
        (addExp net'' x hExp, gen'')

    SFunc (FuncDef outputVar inputVars funcNet) argVars ->
      let
        -- Variables that will need to be renamed
        innerVars = Set.delete outputVar $ Map.keysSet funcNet

        -- Returns max variable in the network
        maxNetV = Set.findMax . netVars

        -- The first variable to be renamed.
        -- We want the max of both nets so that we don't have conflicts when
        -- renaming the funcNet vars.
        nextV = succ $ max (maxNetV net) (maxNetV funcNet)

        -- funcNet with inner variabes renamed
        (renamedInner, _) = Set.foldr
                     (\var (net, nV) -> (renameVarNet net var nV, succ nV))
                     (funcNet, nextV) innerVars

        renamedInputs = List.foldl'
          (\net (inputV, argV) -> renameVarNet net inputV argV)
          renamedInner
          (zip inputVars argVars)

        outputExp = getExp renamedInputs outputVar

        newNet = addAssignments net $
                 Map.insert x outputExp $
                 Map.delete outputVar renamedInputs
      in
        sample newNet x gen
