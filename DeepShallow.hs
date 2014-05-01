-- |Functions for transitioning between deep and shallow expressions
module DeepShallow
( deepToShallow
) where

import Expression
import DeepExpression
import ShallowExpression
import qualified Data.Map as Map

-- |Transforms a deep network of string variables into a shallow network of
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
