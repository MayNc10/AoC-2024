{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Day5 (day5) where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Debug.Trace
import System.IO
import Control.Monad

day5 = do
    contents <- readFile "input/day5.txt"
    let line_arr = lines contents
    let orderings = orders line_arr
    let updates = updatesList line_arr
    print (part1 orderings updates)
    print (part2 orderings updates)

part1 :: [(Int, Int)] -> [[Int]] -> Int
part1 orderings updates = sum (map (\l -> l !! div (length l) 2) (filter (isCorrect orderings) updates))

part2 :: [(Int, Int)] -> [[Int]] -> Int
part2 orderings updates = sum (map (\l -> fixUpdate orderings l !! div (length l) 2) (filter (not . isCorrect orderings) updates))

isCorrect :: [(Int, Int)] -> [Int] -> Bool
isCorrect orderings [] = True
isCorrect orderings update = all (\it -> (it, head update) `notElem` orderings ) (tail update) && isCorrect orderings (tail update)

-- credit to olive for this one
fixUpdate :: [(Int, Int)] -> [Int] -> [Int]
fixUpdate orderings = sortBy (\num1 num2 ->
    if (num2, num1) `elem` orderings then GT
    else if (num1, num2) `elem` orderings then LT
    else EQ)

orders :: [String] -> [(Int, Int)]
orders lines = map ((\l -> (read (head l), read (l !! 1))) . splitOn "|") (filter  ('|' `elem`) lines)

updatesList :: [String] -> [[Int]]
updatesList lines =  map (map read . splitOn ",") (filter  (',' `elem`) lines)