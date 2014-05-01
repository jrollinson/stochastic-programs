-- |Defines the basic expression typeclass and associated functions
module Expression
( -- * Classes
  Expression(..)
  -- * Types
, Network
  -- * Functions
, safeGetExp
, getExp
, addExp
, uses
, usedSet
, usedNetwork
, seenBy
, seenBySet
, addAssignments
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils

-- |Typeclass for expressions
class Expression exp where
  -- | Constant expression for true
  trueExp :: exp var

  -- | Constant expression for false
  falseExp :: exp var

  -- | Returns variables in an expression
  expressionVars :: exp var -> [var]


-- | A network of expressions
type Network exp var = Map.Map var (exp var)


-- | Safely gets expression out of a network (never causes exception)
safeGetExp :: (Ord var) => Network exp var -> var -> Maybe (exp var)
safeGetExp net x = Map.lookup x net

-- | Gets expression out of a network
getExp :: (Ord var) => Network exp var -> var -> exp var
getExp net x = net Map.! x

-- | Adds expression to a network
addExp :: (Ord var) => Network exp var -> var ->  exp var -> Network exp var
addExp net x exp = Map.insert x exp net

-- | Returns whether x uses y in net
-- x uses y if x = y or a variable in the expression for x uses y.
uses :: (Eq v, Ord v, Expression exp) => Network exp v -> v -> v -> Bool
uses net x y = if x == y
               then True
               else 
                 let
                   expVars = expressionVars $ getExp net x
                 in
                   any (\z -> uses net z y) expVars

-- |Returns a set of variables used by variables in the given set in given
-- network
usedSet :: (Eq v, Ord v, Expression exp) => Network exp v -> Set.Set v 
        -> Set.Set v
usedSet net vs =
    let
      -- Variables in the network
      vars = Map.keysSet net

      -- Set of sets of used variables
      setSetUsed = Set.map (\var -> Set.filter (uses net var) vars) vs

       -- Flattens to one set.
    in unions setSetUsed


-- | Returns subset of network of variables used by varibales in v
usedNetwork :: (Ord v, Expression exp) => Network exp v -> Set.Set v
            -> Network exp v
usedNetwork net v =
  let s = usedSet net v
  in Map.filterWithKey (\k _ -> Set.member k s) net

-- | Set of variables seen by y above x in network net.
seenBy :: (Eq v, Ord v, Expression exp) => Network exp v -> v -> v -> Set.Set v
seenBy net y x
    | uses net x y = Set.singleton y
    | otherwise =
        -- Variables in y's expressions
        let 
          vars = expressionVars $ getExp net y 
        in
          -- Unions the set of varibles seen by each v in vars that is not x.
          Set.unions
          $ map (\z -> seenBy net z x)
          $ filter (/=x) vars

-- | Set of variables seen by variables in given set in network net.
seenBySet :: (Eq v, Ord v, Expression exp) => Network exp v -> Set.Set v -> v 
          -> Set.Set v
seenBySet net vs x = unions $ Set.map (\y -> seenBy net y x) vs

-- | Unions the two networks, with second values on top
addAssignments :: (Ord v) => Network a v -> Network a v ->
                             Network a v
addAssignments = Map.unionWith (\n m -> m)
