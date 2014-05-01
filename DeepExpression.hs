-- |Defines deep expression type
module DeepExpression
( DeepExpression(..)
, DeepExpVar(..)
, DeepNetwork
) where

import Expression
import Distribution (Probability)

-- |Type used for tags in data structures and index expressions
type Tag = String

-- |Defines either expression of variable.
type DeepExpVar v = Either (DeepExpression v) v

-- |A deep expression can have nested expressions
data DeepExpression v = DDataStruct Tag [(DeepExpVar v)]
                      | DIndex (DeepExpVar v) Int
                      | DIf (DeepExpVar v) (DeepExpVar v) (DeepExpVar v)
                      | DFlip Probability
                      deriving (Eq, Ord, Show)

-- |Creates a type for a network of deep expressions
type DeepNetwork var = Network DeepExpression var

-- |Turns a DeepExpVar into the variables in it.
-- Mutually recurs with vars
toVars :: DeepExpVar v -> [v]
toVars (Left exp) = vars exp
toVars (Right v) = [v]

-- |Gets variables used in a deep expression
vars :: DeepExpression v -> [v]
vars (DDataStruct _ eVars) = concat $ map toVars eVars
vars (DIndex evar _) = toVars evar
vars (DIf ifVar thenVar elseVar) = concat $ map toVars [ifVar, thenVar, elseVar]
vars (DFlip a) = []


instance Expression DeepExpression where
  trueExp = DDataStruct "True" []
  falseExp = DDataStruct "False" []
  expressionVars = vars
