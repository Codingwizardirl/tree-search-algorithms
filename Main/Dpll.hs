-- Propositional Inference

module Main where

import System.Environment
import Inf2d
import SatParser

main = do
        args <- getArgs
        sat <- getExample (args!!0)
        case (map (\x -> (x!!0)) (filter (\x -> x /= [[]]) sat)) of
         [] -> putStrLn "Not a valid Sat problem"
         sentence -> test sentence


test sentence = do 
                 putStrLn "Using DPLL..." 
                 let dpllRes = dpllSatisfiable sentence
                 putStrLn ("dpll: Sentence is " ++ (show (fst dpllRes)) ++ " and number of backtrack calls made is " ++ (show (snd dpllRes)))


