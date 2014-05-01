-- | Defines shallow expression type
module ShallowExpression
( ShallowExpression(..)
, ShallowNetwork
, renameVarNet
) where

import Expression
import Distribution (Probability)
import qualified Data.Map as Map

-- | Type of tags of data structures and index expressions
type Tag = String

-- | A shallow expression only has one level of expressions
data ShallowExpression v
  = SDataStruct Tag [v] -- ^ A data structure with a tag and variables.
  | SIndex v Int -- ^ An index into data structure a variable
  | SIf v v v -- ^ A conditional statement
  | SFlip Probability -- ^ True or False based on a probability
  | SFunc (FuncDef ShallowExpression v) [v]
    -- ^ A function made out of a function definition and a mapping of variables
    -- to variables in the function definition.
    -- The mapping is of variables in the function network to variables in the
    -- overall network.
  deriving (Eq, Ord, Show)


instance Expression ShallowExpression where
    trueExp = SDataStruct "True" []
    falseExp = SDataStruct "False" []

    -- | Returns the expression variables of a shallow expression
    expressionVars (SDataStruct _ vars) = vars
    expressionVars (SIndex var _) = [var]
    expressionVars (SIf ifV thenV elseV) = [ifV, thenV, elseV]
    expressionVars (SFlip _) = []
    expressionVars (SFunc funcDef args) = args

  -- | Type for a network of shallow expressions
type ShallowNetwork var = Network ShallowExpression var

-- | Maps function to all variables, except those enclosed in functions
mapVarExpNotFunc :: (a -> a) -> ShallowExpression a -> ShallowExpression a
mapVarExpNotFunc f (SDataStruct tag vs) = SDataStruct tag (map f vs)
mapVarExpNotFunc f (SIndex v i) = SIndex (f v) i
mapVarExpNotFunc f (SIf ifV thenV elseV) = SIf (f ifV) (f thenV) (f elseV)
mapVarExpNotFunc f (SFlip p) = SFlip p
mapVarExpNotFunc f (SFunc funcDef args) = SFunc funcDef (map f args)

-- | Renames a variable in an expression
renameVarExp :: (Eq v) => v -> v -> ShallowExpression v -> ShallowExpression v
renameVarExp oldVar newVar exp =
    mapVarExpNotFunc
    (\v -> if v == oldVar then newVar else v)
    exp

-- | Renames a variable in a network
renameVarNet :: (Ord v, Eq v) => ShallowNetwork v -> v -> v -> ShallowNetwork v
renameVarNet net oldVar newVar =
  let
    -- Renames the variable in all expressions
    renamedExps = Map.map (renameVarExp oldVar newVar) net
  in
    -- Sets new variable to point to expression of old variable and removes old
    -- variable.
    case Map.lookup oldVar net of
      Just exp -> Map.insert newVar exp $ Map.delete oldVar renamedExps
      Nothing -> renamedExps
