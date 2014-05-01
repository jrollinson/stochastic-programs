-- |Defines shallow expression type
module ShallowExpression
( ShallowExpression(..) 
, ShallowNetwork
) where

import Expression
import Distribution (Probability)

-- | Type of tags of data structures and index expressions
type Tag = String 

-- |A shallow expression only has one level of expressions
data ShallowExpression v 
  = SDataStruct Tag [v] -- ^ A data structure with a tag and variables.
  | SIndex v Int -- ^ An index into data structure a variable
  | SIf v v v -- ^ A conditional statement
  | SFlip Probability -- ^ True or False based on a probability
  deriving (Eq, Ord, Show)


instance Expression ShallowExpression where
    trueExp = SDataStruct "True" []
    falseExp = SDataStruct "False" []

    -- | Returns the expression variables of a shallow expression
    expressionVars (SDataStruct _ vars) = vars
    expressionVars (SIndex var _) = [var]
    expressionVars (SIf ifV thenV elseV) = [ifV, thenV, elseV]
    expressionVars (SFlip _) = []

-- |Type for a network of shallow expressions
type ShallowNetwork var = Network ShallowExpression var
