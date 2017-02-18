-- Propositional Inference
-- Informatics 2D
-- Assignment 1
-- 2016-2017
--
-- MATRIC NUMBER HERE:
--
-- Please remember to comment ALL your functions.

---------------------------------------------------------------------------
---------------------------------------------------------------------------

-- Module declaration and imports
-- DO NOT EDIT THIS PART --

module Inf2d where

import System.Random
import Data.Maybe
import Data.List
import Prop

---------------------------------------------------------------------------
---------------------------------------------------------------------------

-- TASK 1 : Some simple functions

lookupAssignment :: Symbol -> Model -> Maybe Bool
lookupAssignment = undefined

negateSymbol :: Symbol -> Symbol
negateSymbol = undefined

isNegated :: Symbol -> Bool
isNegated = undefined

getUnsignedSymbol :: Symbol -> Symbol
getUnsignedSymbol = undefined

getSymbols :: [Sentence] -> [Symbol]
getSymbols = undefined

-- Whether symbol is satisfied by the model
satisfiesSymbol :: Model -> Symbol -> Bool
satisfiesSymbol [] _ = False
satisfiesSymbol model symbol =  case valueOf model $ variableOf symbol of
                Nothing -> False
                Just variable -> (polarity symbol) == variable

-- Whether clause is satisfied by the model
satisfiesClause :: Model -> Clause -> Bool
satisfiesClause [] _ = False
satisfiesClause model clause = any (satisfiesSymbol model) clause

-- Whether sentence is satisfied by the model
satisfiesSentence :: Model -> Sentence -> Bool
satisfiesSentence [] _ = False
satisfiesSentence model sentence = all (satisfiesClause model) sentence

-- Whether symbol is falsified by the model
falsifiesSymbol :: Model -> Symbol -> Bool
falsifiesSymbol [] _ = False
falsifiesSymbol model symbol = case valueOf model $ variableOf symbol of
                Nothing -> False
                Just variable -> (polarity symbol) /= variable

-- Whether clause is falsified by the model
falsifiesClause :: Model -> Clause -> Bool
falsifiesClause [] _ = False
falsifiesClause model clause = all (falsifiesSymbol model) clause

-- Whether sentence is falsified by the model
falsifiesSentence :: Model -> Sentence -> Bool
falsifiesSentence model sentence = any (falsifiesClause model) sentence

---------------------------------------------------------------------------
---------------------------------------------------------------------------

-- TASK 2 : WalkSat
-- List of unsatisfied clauses by the model
unsatClauses :: Model -> Sentence -> [Clause]
unsatClauses model sentence = filter (falsifiesClause model) sentence

-- Returns the number of unsatisfied clauses in the sentence given if the variable given is flipped
numUnsatClauses :: Model -> Sentence -> Variable -> Int
numUnsatClauses model sentence variable = length $ unsatClauses (flipSymbol model variable) sentence

-- Flip all variables in the list and get the one with the minimum amount of unsatisfied clauses -
-- which yields maximum amount of satisfied clauses
maxSatClauses :: Model -> Sentence -> [Variable] -> Variable
maxSatClauses model sentence varList = variable
              where (unsatClauses,variable) = minimum [ (numUnsatClauses model sentence variable, variable) | variable <- varList ]

---------------------------------------------------------------------------

-- Implementation of the WalkSat algorithm.
-- DO NOT EDIT THIS PART --

walkSatRecursion :: RandomGen g => g -> Sentence -> Model -> Float -> Int -> (Maybe (Model,Int),g)
walkSatRecursion gen sentence model _ 0
                 | satisfiesSentence model sentence = (Just (model,0),gen)
                 | otherwise = (Nothing,gen)
walkSatRecursion gen sentence model prob n
                 | satisfiesSentence model sentence = (Just (model,n),gen)
                 | rchoice = walkSatRecursion gen3 sentence flipRandom prob (n-1)
                 | otherwise = walkSatRecursion gen3 sentence flipMaxSat prob (n-1)
                           where (rchoice,gen1) = randomChoice gen prob (1.0::Float)
                                 (clause,gen2) = randomElem gen1 (unsatClauses model sentence)
                                 atms = variablesOfClause clause
                                 (rvariable,gen3) = randomElem gen2 atms
                                 mvariable = maxSatClauses model sentence atms
                                 flipRandom = flipSymbol model rvariable
                                 flipMaxSat = flipSymbol model mvariable

walkSat :: Sentence -> Float -> Int -> IO (Maybe (Model,Int))
walkSat sentence prob n = do
      gen <- getStdGen
      let (rassign,gen') = (randomAssign gen (variablesOfSentence sentence))
      let (res,gen'') = walkSatRecursion gen' sentence rassign prob n
      setStdGen (gen'')
      putStrLn $ show res
      return res

---------------------------------------------------------------------------
---------------------------------------------------------------------------

-- TASK 3 : DPLL

-- SECTION 7.1 : Tautology Deletion

-- Checks if clause is a tautology
isTautology :: Clause -> Bool
isTautology clause = or [isSymbol (complement symbol) clause | symbol <- clause]

-- Removes tautologies from sentence using the above helper function
removeTautologies :: Sentence -> Sentence
removeTautologies sentence = filter (not . isTautology) sentence


---------------------------------------------------------------------------

-- SECTION 7.2 : Pure Symbol Heuristic

-- Checks if any of the variables in the list produce a pure symbol and returns the first one that does.
findPureSymbol :: [Variable] -> Sentence -> Model -> Maybe (Variable, Bool)
findPureSymbol variables sentence model = if null var then Nothing else Just (var, sign)
                  where LTR (sign, var) = head $ concat [ maybeToList (isPure (LTR (True, variable)) sentence model) ++
                                                        maybeToList (isPure (LTR (False, variable)) sentence model)
                                                        | variable <- variables ]


-- Checks if symbol is pure. If symbol is pure it returns the symbol. Otherwise returns Nothing.
-- Ignores clauses which are satisfied by the model.
isPure :: Symbol -> Sentence -> Model -> Maybe Symbol
isPure symbol sentence model =  if unique then Just symbol else Nothing
              where unique = and [not (isSymbol (complement symbol) clause) | clause <- sentence, not $ satisfiesClause model clause]

-- SECTION 7.3 : Unit Clause Heuristic

findUnitClause :: Sentence -> Model -> Maybe (Variable, Bool)
findUnitClause = undefined

-- SECTION 7.4 : Early Termination

earlyTerminate :: Sentence -> Model -> Bool
earlyTerminate = undefined

---------------------------------------------------------------------------

-- SECTION 7.5 : DPLL algorithm

dpll :: (Node -> Variable) -> [Node] -> Int -> (Bool, Int)
dpll = undefined

---------------------------------------------------------------------------

-- Provided choice function and dpll initialisation
-- DO NOT EDIT THIS PART --

firstVariable :: Node -> Variable
firstVariable (_, ([],_)) = undefined
firstVariable (_, (h:_,_)) = h

dpllSatisfiable :: Sentence -> (Bool, Int)
dpllSatisfiable sentence = dpll firstVariable [(removeTautologies sentence, (variablesOfSentence sentence, []))] 0


---------------------------------------------------------------------------

-- TASK 4

variableSelectionHeuristic :: Node -> Variable
variableSelectionHeuristic = undefined

---------------------------------------------------------------------------

-- Provided dpllSatisfiablev2
-- DO NOT EDIT THIS PART --

dpllSatisfiablev2 :: Sentence -> (Bool, Int)
dpllSatisfiablev2 sentence = dpll variableSelectionHeuristic [(removeTautologies sentence, (variablesOfSentence sentence, []))] 0

---------------------------------------------------------------------------

-- Examples of type Sentence which you can use to test the functions you develop

f2 = [[LTR (True,"p"), LTR (True,"q")], [LTR (True,"p"), LTR (True,"q"), LTR (True,"z")], [LTR (False,"z"), LTR (False,"w"), LTR (True,"k")], [LTR (False,"z"), LTR (False,"w"), LTR (True,"s")], [LTR (True,"p"), LTR (False,"q")]]
f3 = [[LTR (True,"k"), LTR (False,"g"), LTR (True,"t")], [LTR (False,"k"), LTR (True,"w"), LTR (True,"z")], [LTR (True,"t"), LTR (True,"p")], [LTR (False,"p")], [LTR (True,"z"), LTR (True,"k"), LTR (False,"w")], [LTR (False,"z"), LTR (False,"k"), LTR (False,"w")], [LTR (False,"z"), LTR (True,"k"), LTR (True,"w")]]
f4 = [[LTR (True,"p")], [LTR (False,"p")]]
f5 = [[LTR (True,"p"), LTR (False,"q")], [LTR (True,"p"), LTR (True,"q")]]
f6 = [[LTR (True,"p"), LTR (False,"q")], [LTR (True,"q")]]
f7 = [[LTR (True,"p"), LTR (False,"q")], [LTR (True,"q"), LTR (True,"k")], [LTR (True,"q"), LTR (False,"p"), LTR (False,"k")]]

-- Models to test functions
model1 = [AS ("z", True)]
model2 = []
---------------------------------------------------------------------------

-- TASK 5 : Evaluation

-- TABLE 1 : WalkSat
{-
          |  p=0  | p=0.5 |  p=1  |
----------+-------+-------+-------+
Sat01.cnf |       |       |       |
Sat02.cnf |       |       |       |
Sat03.cnf |       |       |       |
Sat04.cnf |       |       |       |
Sat05.cnf |       |       |       |
Sat06.cnf |       |       |       |
Sat07.cnf |       |       |       |
Say08.cnf |       |       |       |
Say09.cnf |       |       |       |
Say10.cnf |       |       |       |
----------+-------+-------+-------+

-- TABLE 2 : DPLL and DPLLv2

          | DPLL   | DPLLv2 |
----------+--------+--------+
Sat01.cnf |        |        |
Sat02.cnf |        |        |
Sat03.cnf |        |        |
Sat04.cnf |        |        |
Sat05.cnf |        |        |
Sat06.cnf |        |        |
Sat07.cnf |        |        |
Say08.cnf |        |        |
Say09.cnf |        |        |
Say10.cnf |        |        |
----------+--------+--------+
-}
---------------------------------------------------------------------------

-- TASK 6 : REPORT

-- Do NOT exceed 1.5 A4 pages of printed plain text.
-- Write your report HERE:
{-

Report goes in here!!

-}

---------------------------------------------------------------------------
---------------------------------------------------------------------------
