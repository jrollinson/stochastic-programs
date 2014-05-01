-- |Functions for turning networks into distributions
module DistEval
( dist
, dist'
, pEval
) where

import Expression
import ShallowExpression
import Distribution
import qualified Data.Set as Set
import qualified Data.Map as Map


-- |Naive distribution builder.
dist :: (Eq v, Ord v) => ShallowNetwork v -> v -> Dist (ShallowNetwork v)
dist net x = case getExp net x of

    SDataStruct _ _ -> certainly net

    SIndex y i ->
      let
        nDist = dist net y

        xDists net' = case getExp net' y of
          SDataStruct t l ->
            let
              z = (l !! i)
              nDist' = dist net' z
            in
              dMap (\n -> addExp n x (getExp n z)) nDist'

          _ -> certainly (addExp net' x falseExp)
      in
        flattenDist $ dMap xDists nDist

    SFlip p -> relative [ (addExp net x trueExp, p)
                        , (addExp net x falseExp, 1-p)
                        ]

    SIf y z w ->
      let
        -- First we compute the distribution for y
        yDistribution = dist net y

        -- Computes the distribution for a network with given y value.
        yDists n' =
            let
              h = if (getExp n' y == trueExp) then z else w
              hDist = dist n' h
            in
              dMap (\n -> addExp n x (getExp n h)) hDist

        -- A distribution of distributions of networks
        dists = dMap yDists yDistribution

      in flattenDist dists


-- |Uses dist to return a distribution over set V
dist' :: (Show v, Ord v) => ShallowNetwork v -> v -> Set.Set v
      -> Dist (ShallowNetwork v)
dist' net x vs =
    let
      d = dist net x
      shrunkD = dMap (\n -> usedNetwork n vs) d
    in
      combineDist shrunkD

-- |Smartly creates a distribution over variables in vs
pEval :: (Ord v) => ShallowNetwork v -> v -> Set.Set v -> Dist (ShallowNetwork v)
pEval net x vs = pHelp (usedNetwork net (Set.singleton x)) x vs
    where
      pHelp net x vs = case getExp net x of

          SDataStruct t v -> certainly (usedNetwork net vs)

          SFlip p -> relative [ (Map.singleton x trueExp, p)
                              , (Map.singleton x falseExp, 1-p)]

          SIf y z w ->
            let
              -- TODO: This looks scary!
              yDist = pEval net y (seenBySet net vs y)

              -- Calculates distribution for network in yDist
              mDist m =
                  let
                    n' = addAssignments net m
                    h = if (getExp n' y == trueExp) then z else w
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
