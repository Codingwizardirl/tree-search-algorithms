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
falsifiesSentence [] _ = False
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

-- Removes clauses which are satisfied by the model from the sentence
simplifySentence :: Model -> Sentence -> Sentence
simplifySentence model sentence = [clause | clause <- sentence, satisfiesClause model clause == False]


-- Checks if the variable produces a pure symbol.
-- Function filters all symbols in the sentence produced by the passed variable and checks if they all are the same symbol (have the same polarity)
isPureVar :: Variable -> Sentence -> Model -> Maybe Symbol
isPureVar variable sentence model
                            | length varList == 1 = Just (head varList)
                            | otherwise = Nothing
                    where
                      varList = nub $ concat $ map (\clause -> filterClause variable clause) sentence
                      filterClause variable clause = filter (\symbol -> variableOf symbol == variable) clause

-- Checks if any of the variables in the list produce a pure symbol and returns the values to assign to the first one of them.
-- Function first simplifies the sentence by first removing the clauses satisfied by the model.
-- Returns Nothing if no pure symbols are found.
findPureSymbol :: [Variable] -> Sentence -> Model -> Maybe (Variable, Bool)
findPureSymbol variables sentence model
                          | isNothing pureSym = Nothing
                          | otherwise = Just(var, value)

          where
          pureSym = listToMaybe [ fromJust $ purePredicate variable
                                | variable <- variables,
                                  variable `elem` prunedVaribles && isJust (purePredicate variable)
                                ]
          purePredicate variable = isPureVar variable simplifiedSentence model
          prunedVaribles = variablesOfSentence simplifiedSentence
          simplifiedSentence = simplifySentence model sentence
          var = variableOf $ fromJust pureSym
          value = polarity $ fromJust pureSym


-- SECTION 7.3 : Unit Clause Heuristic

-- Removes symbols in the clause which are falsified by the model
simplifyClause :: Model -> Clause -> Clause
simplifyClause _ [] = []
simplifyClause [] clause = clause
simplifyClause model clause = [symbol | symbol <- clause, falsifiesSymbol model symbol == False]


-- Checks if any clause in the sentence is unit clause and returns the value to assign to the first one of them.
-- Returns Nothing if no unit clauses are found.
findUnitClause :: Sentence -> Model -> Maybe (Variable, Bool)
findUnitClause sentence model
                  | isNothing unitClause = Nothing
                  | otherwise = Just(variable, value)
          where
          unitClause = find unitPredicate simplifiedSentence
          unitPredicate clause = length (simplifyClause model clause) == 1 && not (satisfiesClause model $ simplifyClause model clause)
          simplifiedSentence = simplifySentence model sentence
          variable = variableOf $ head $ simplifyClause model $ fromJust unitClause
          value = polarity $ head $ simplifyClause model $ fromJust unitClause


-- SECTION 7.4 : Early Termination

-- Checks whether sentence is satisfied or falsified by the given model
earlyTerminate :: Sentence -> Model -> Bool
earlyTerminate sentence model = falsifiesSentence model sentence || satisfiesSentence model sentence

---------------------------------------------------------------------------

-- SECTION 7.5 : DPLL algorithm
-- Code follow pseudo code given in the book.
-- Algorithm first checks for early termination. Then it finds all pure symbols and assigns them to true.
-- It finds all unit clauses and assigns all symbols in there to true.
-- If sentence isn't satisfied it choses a variable using heuristic and branches on that variable.
dpll :: (Node -> Variable) -> [Node] -> Int -> (Bool, Int)
dpll heuristic [] i = (False, i)
dpll heuristic ((sentence, (variables, model)):xs) i
              | earlyTerminate sentence model  =
                case satisfiesSentence model sentence of
                  True -> (True, i)
                  False -> dpll heuristic xs (i + 1)
              | otherwise =
                case findPureSymbol variables sentence model of
                  Just (variable, value) -> dpll heuristic ((sentence,(delete variable variables, assign model variable value)):xs) i
                  Nothing ->
                    case findUnitClause sentence model of
                      Just (variable, value) -> dpll heuristic ((sentence,(delete variable variables, assign model variable value)):xs) i
                      Nothing ->
                          dpll heuristic (nodeTrue:nodeFalse:xs) i
                              where
                                node = (sentence, (variables, model))
                                chosenVariable = heuristic node
                                newVariables = delete chosenVariable variables
                                nodeTrue = (sentence, ( newVariables, assign model chosenVariable True))
                                nodeFalse = (sentence, ( newVariables, assign model chosenVariable False))




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

-- Applies the formula (f(x) + f(-x))*2^k + f(x)*f(-x) for k = 4
-- This is formula generally used with the MOM heuristic
-- We try to maximize this function
applyFormula :: Float -> Float -> Float
applyFormula f1 f2 = (f1 + f2)*(2^4) + f1*f2


-- Returns ratio of number of occurences of the symbol to the length of the clause it occurs in (relative frequency)
-- This normalizes the frequency and gives higher weight to smaller clauses with high occurances
frequencyRatio :: Symbol -> Clause -> Float
frequencyRatio symbol clause =  (fromIntegral $ length $ filter(\sym -> sym == symbol) clause) / (fromIntegral $ length clause)

-- Given a variable it first calculates the relative frequencies for the positive and negative symbol
-- created by the given variable and then applies the formula we wish to maximise using these numbers.
scoreVariable :: Variable -> Sentence -> Float
scoreVariable variable sentence = applyFormula posRatio negRatio
                      where
                      (posRatio,negRatio) = foldl' (\(a,b) (c,d)-> (a+c, c+d)) (0,0) [(frequencyRatio posSymbol clause, frequencyRatio negSymbol clause) | clause <- sentence ]
                      posSymbol = LTR(True, variable)
                      negSymbol = LTR(False, variable)

-- Heuristic acts on the basis of the Maximum Occurrences on clauses of Minimum size (MOM’s) heuristic
-- Calls the scoreVariable function on every variable in the list and returns the one that maximises the function
-- given in applyFormula.
variableSelectionHeuristic :: Node -> Variable
variableSelectionHeuristic (sentence, (variables, model)) = chosenVariable
                          where
                            (score, chosenVariable) = maximum [(scoreVariable variable simplifiedSentence, variable) | variable <- variables]
                            simplifiedSentence = simplifySentence model sentence

---------------------------------------------------------------------------

-- Provided dpllSatisfiablev2
-- DO NOT EDIT THIS PART --

dpllSatisfiablev2 :: Sentence -> (Bool, Int)
dpllSatisfiablev2 sentence = dpll variableSelectionHeuristic [(removeTautologies sentence, (variablesOfSentence sentence, []))] 0

---------------------------------------------------------------------------

-- Examples of type Sentence which you can use to test the functions you develop
f1 = [[LTR (True,"p"), LTR (True,"q")], [LTR (True,"p"), LTR (False,"p")], [LTR (True,"q")]]
f2 = [[LTR (True,"p"), LTR (True,"q")], [LTR (True,"p"), LTR (True,"q"), LTR (True,"z")], [LTR (False,"z"), LTR (False,"w"), LTR (True,"k")], [LTR (False,"z"), LTR (False,"w"), LTR (True,"s")], [LTR (True,"p"), LTR (False,"q")]]
f3 = [[LTR (True,"k"), LTR (False,"g"), LTR (True,"t")], [LTR (False,"k"), LTR (True,"w"), LTR (True,"z")], [LTR (True,"t"), LTR (True,"p")], [LTR (False,"p")], [LTR (True,"z"), LTR (True,"k"), LTR (False,"w")], [LTR (False,"z"), LTR (False,"k"), LTR (False,"w")], [LTR (False,"z"), LTR (True,"k"), LTR (True,"w")]]
f4 = [[LTR (True,"p")], [LTR (False,"p")]]
f5 = [[LTR (True,"p"), LTR (False,"q")], [LTR (True,"p"), LTR (True,"q")]]
f6 = [[LTR (True,"p"), LTR (False,"q")], [LTR (True,"q")]]
f7 = [[LTR (True,"p"), LTR (False,"q")], [LTR (True,"q"), LTR (True,"k")], [LTR (True,"q"), LTR (False,"p"), LTR (False,"k")]]

-- Models to test functions
model1 = [AS ("z", True)]
model2 = [AS ("p", False), AS("t", True), AS("g", False)]
model3 = [AS ("p", False), AS("g", False)]
model4 = []
---------------------------------------------------------------------------

-- TASK 5 : Evaluation

-- TABLE 1 : WalkSat
{-
          |  p=0  | p=0.5 |  p=1  |
----------+-------+-------+-------+
Sat01.cnf |  56   |  255  |  289  |
Sat02.cnf | Fail  |  Fail |  Fail |
Sat03.cnf |  45   |  140  |  304  |
Sat04.cnf | Fail  |  Fail |  Fail |
Sat05.cnf | Fail  |  592  |  Fail |
Sat06.cnf | Fail  |  Fail |  Fail |
Sat07.cnf | Fail  |  Fail |  Fail |
Say08.cnf |  48   |  133  |  300  |
Say09.cnf | Fail  |  Fail |  Fail |
Say10.cnf |  34   |  573  |  Fail |
----------+-------+-------+-------+

-- TABLE 2 : DPLL and DPLLv2

          | DPLL   | DPLLv2 |
----------+--------+--------+
Sat01.cnf |   0    |   0    |
Sat02.cnf |   93   |   27   |
Sat03.cnf |   0    |   0    |
Sat04.cnf |   337  |   128  |
Sat05.cnf |   260  |   95   |
Sat06.cnf |   943  |   101  |
Sat07.cnf |   624  |   5    |
Say08.cnf |   0    |   0    |
Say09.cnf |   300  |   173  |
Say10.cnf |   158  |   19   |
----------+--------+--------+
-}
---------------------------------------------------------------------------

-- TASK 6 : REPORT

-- Do NOT exceed 1.5 A4 pages of printed plain text.
-- Write your report HERE:
{-

1. WalkSAT and DPLL

a) Results obtained –
WalkSAT - WalkSAT manage to find solutions only to the more simple SAT problems. It failed to find a model for 5 of the provided problems, while only 3 of them were not satisfiable. Given the limit of 600 flips the algorithm was more reliant on favourable randomized model and luck in flipping during the “random walk” in the situations where it occurred.  Having said that and the fact that the algorithm is not complete made all the results expected.

DPLL -  DPLL managed to deduce whether a sentence is satisfiable or not in a reasonable time frame. The improved heuristic version of the algorithm performed better in all of the tests in both run time and number of time it branched. Once again results were expected.

b)
Max flips – Simply sets and upper limit on run time. In the given set a bigger number of maximum flips would have affected the algorithm positively. WalkSAT on the set of problems ran in more than reasonable time, however, failed to find model for satisfiable problems because they required more than 600 flips from the randomized model.
A good balance between run time and found solutions in my experience is somewhere around 2500.

p – The probability of performing a “random walk” has direct correlation on the run time of the algorithm given fixed maximum flips. There is a trade off between reliability and run time. For lower probability the algorithm runs slower as it has to make more educated guesses rather than just randomly picking a variable to flip. However, it produces resolutions of the SAT problems more regularly. On bigger values of p the algorithm is faster, however, produces less reliable results. It is often the case that because variables are picked randomly to be flipped the maximum number of flips is reached way before a model is produced.
For the given fixed maximum flips of 600 a p in the range between 0 and 0.33 is favourable as it produces results in less flips and more reliably. For bigger number of maximum flips p in the range between 0.5 and 0.7 is also a good choice in my experience on the given set of problems.

c)
While both are algorithms for solving satisfiability problems they are vastly different.

WalkSAT is a local search and starts from a randomly generated model and flips variable truth values until it finds a solution or exceeds a number of flips.
DPLL is a backtrack search algorithm, which starts from an empty model and assigns truth values to variables as it progresses with the help of different techniques, such as finding pure symbols and unit clauses and assigning them true in the model.

Main difference between the two algorithms is the fact that DPLL is complete while WalkSAT is not. While DPLL is always able to tell whether a problem is satisfiable (given enough time), WalkSAT is only able to say whether it can figure out a model for the problem in the time limit it has been given. In other words, DPLL is complete algorithm while WalkSAT is not.


2. Improving the efficiency of DPLL

a) My idea came from reading the course’s textbook and a couple of academic papers on branching heuristics. Heuristic is based on the Maximum Occurrences on clauses of Minimum size (MOM’s) heuristic.
More specifically resources that I read and inspired me to come up with my idea were:
Branching Heuristics, Matt Ginsberg(2004)
(https://www.cs.cmu.edu/afs/cs/project/jair/pub/volume21/dixon04a-html/node8.html)
The Impact of Branching Heuristics in Propositional Satisfiability Algorithms,Joao Marques-Silva(https://pdfs.semanticscholar.org/c4d3/992a801af2862011e03eeabfdaa5a926393f.pdf)
Fundaments of Branching Heuristics, Oliver Kullmann(http://cs.swan.ac.uk/~csoliver/Artikel/p01c07_heu.pdf)

b)
I implemented the heuristic by picking variable out of the list of variables in the node by choosing the  one that maximizes a score function based on MOM heuristic.

Formula: (f(x) + f(-x))*2^k + f(x)*f(-x),  for k = 4 in my case.

Here f(x) is a relative score of a symbol “x” by occurring in a clause. f(x) is a frequency ratio, calculated by dividing the number of times symbol “x” is found in clause C by the length of C.
This gives more importance to the symbol found in shorter clauses.

c) Heuristic improves DPLL’s performance by choosing a variable by the formula above, where we maximize the importance of maximum occurrences in clauses of minimal length.
This variable is most likely to cause a chain of discovery of unit clauses, leading to a model. I observed a major improvement in both run time and branching factor of the algorithm given the new heuristic.

d) When finding unit clauses and pure symbols, the search

-}

---------------------------------------------------------------------------
---------------------------------------------------------------------------
