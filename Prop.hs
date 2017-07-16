-- Propositional Inference

module Prop where

import System.Random
import Data.Maybe
import Data.List

---------------------------------------------------------------------------------

-- Variable

type Variable = String

---------------------------------------------------------------------------------

-- Symbol

newtype Symbol = LTR (Bool,Variable)

-- Pretty printing
instance Show Symbol where
  show (LTR (True,a)) = a
  show (LTR (False,a)) = "¬" ++ a

-- Comparison 
instance Eq Symbol where
  (LTR a) == (LTR b) = a == b

-- Retrieve the variable involved in the symbol
variableOf :: Symbol -> Variable
variableOf (LTR (_,x)) = x

-- Retrieve the polarity of the symbol
polarity :: Symbol -> Bool
polarity (LTR (x,_)) = x

-- Create the complement of a symbol
complement :: Symbol -> Symbol
complement (LTR (a,b)) = (LTR (not a,b))

---------------------------------------------------------------------------------

-- Clause

type Clause = [Symbol]

-- Whether a symbol can be found in a clause
isSymbol :: Symbol -> Clause -> Bool
isSymbol symbol clause = elem symbol clause

-- The set (ie. no duplicates) of variables in a clause
variablesOfClause :: Clause -> [Variable]
variablesOfClause clause = nub $ (map variableOf clause)

---------------------------------------------------------------------------------

-- Sentence 

type Sentence = [Clause]

-- The set (ie. no duplicates) of variables in a sentence
variablesOfSentence :: Sentence -> [Variable]
variablesOfSentence sentence = variablesOfClause $ concat sentence

---------------------------------------------------------------------------------

-- Assignment

newtype Assignment = AS (Variable, Bool)

-- Pretty printing
instance Show Assignment where
  show (AS (a,v)) = a ++ "=" ++ (show v)

-- Comparison
instance Eq Assignment where
  (AS a) == (AS b) = a == b

-- Retrieve the assigned variable
assignedVariable :: Assignment -> Variable
assignedVariable (AS (x,_)) = x

-- Retrieve the assigned value
assignedValue :: Assignment -> Bool
assignedValue (AS (_,x)) = x

---------------------------------------------------------------------------------

-- Model 

type Model = [Assignment]

-- Delete an assignment
unassign :: Model -> Variable -> Model
unassign m a = [ as | as <- m , not $ a == assignedVariable as]

-- (Re)Assign a value to an variable
assign :: Model -> Variable -> Bool -> Model
assign m a v = (AS (a,v)):(unassign m a)

-- Retrieve Just the assigned value of an variable in a model or Nothing if it is unassigned.
valueOf :: Model -> Variable -> Maybe Bool
valueOf m a = case find (\as -> assignedVariable as == a) m of
         Nothing -> Nothing
         Just as -> Just $ assignedValue as

-- Flip the assigned value of an variable in a model.
flipSymbol :: Model -> Variable -> Model
flipSymbol m a = case valueOf m a of
           Nothing -> m
           Just l -> assign m a (not l)

---------------------------------------------------------------------------------
           
-- Search node
           
type Node = (Sentence, ([Variable], Model))

---------------------------------------------------------------------------------

-- Random generators (used in WalkSat).

randomChoice :: RandomGen g => g -> Float -> Float -> (Bool,g)
randomChoice gen probability max = (value <= probability,newgen)
                                where (value,newgen) = randomR (0.0::Float,max) gen

randomElem :: RandomGen g => g -> [a] -> (a,g)
randomElem gen list = (list !! v,newgen)
                where (v,newgen) = randomR (0,(length list) -1) gen

randomAssign :: RandomGen g => g -> [Variable] -> (Model,g)
randomAssign gen [] = ([],gen)
randomAssign gen (x:xs) = (assign ys x y,newgen)
                where (y,gen') = randomR (False,True) gen
                      (ys,newgen) = randomAssign gen' xs

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
