module Day4 (day4) where

import Data.List
import Data.Maybe
import Debug.Trace
import System.IO
import Control.Monad
day4 = do
    contents <- readFile "input/day4.txt"
    let line_arr = lines contents
    print (part1 line_arr)

part1 :: [String] -> Int
part1 chars = sum (map (`countXmas` chars) [0 .. (length chars * length (head chars) - 1) ])


countXmas :: Int -> [String] -> Int
countXmas idx chars = length (filter (\s -> lineMatch s r_idx c_idx chars) slopes)
    where
        r_idx = div idx (length (head chars))
        c_idx = mod idx (length (head chars))
        slopes = [(1,0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

lineMatch :: (Int, Int) -> Int -> Int -> [String] -> Bool
lineMatch slope r_idx c_idx chars =
    between 0 (r_idx + fst slope * 3) (length chars)
    && between 0 (c_idx + snd slope * 3) (length (head chars))
    && (chars !! r_idx) !! c_idx == 'X'
    && (chars !! (r_idx + fst slope)) !!(c_idx + snd slope) == 'M'
    && (chars !! (r_idx + fst slope * 2)) !!(c_idx + snd slope * 2) == 'A'
    && (chars !! (r_idx + fst slope * 3)) !! (c_idx + snd slope * 3) == 'S'

between :: Int -> Int -> Int -> Bool
between x y z
  |x <= y = y < z
  |otherwise = False

