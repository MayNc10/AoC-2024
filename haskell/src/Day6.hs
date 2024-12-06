module Day6 (day6) where

import Combinatorics
import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.IO
import Control.Monad

day6 = do

    contents <- readFile "input/day6test.txt"
    let line_arr = lines contents
    print (part1 line_arr)
    print (part2 line_arr)

part1 :: [String] -> Int
part1 gmap = length (nub path)
    where path = guardPath gmap (guardLoc gmap) (-1, 0)

part2 :: [String] -> Int
part2 gmap = div (length (trace (show corners) corners)) 2
    where path = guardPath gmap (guardLoc gmap) (-1, 0)
          turns = nub [item | (pos, item) <- zip [0..] path, pos /= length path - 1 && path !! (pos + 1) == item]
          corners = nub [fromJust corner | corner <- map turnTriple (directedPerms turns), isJust corner]

turnTriple :: [(Int, Int)] -> Maybe (Int, Int)
turnTriple list | True = Just (0, 3)
                | otherwise Nothing

guardPath :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
guardPath gmap (ridx, cidx) (vdir, hdir)
                        | outsideMap gmap (ridx + vdir, cidx + hdir) = [pos]
                        | char /= '.' && char /= '^' = pos : guardPath gmap (ridx, cidx) (rightTurn dir)
                        | otherwise = pos : guardPath gmap (ridx + vdir, cidx + hdir) dir
                        where pos = (ridx, cidx)
                              dir = (vdir, hdir)
                              char = gmap !! (ridx + vdir) !! (cidx + hdir)

outsideMap :: [String] -> (Int, Int) -> Bool
outsideMap gmap (ridx, cidx) = (ridx < 0 || ridx >= length gmap) || cidx < 0 || cidx >= length (head gmap)

rightTurn :: (Int,Int) -> (Int, Int)
rightTurn (vdir, hdir) = (hdir, vdir * (-1))

guardLoc :: [String] -> (Int, Int)
guardLoc gmap = head [(ridx, fromJust cidx) | (ridx, cidx) <- zipWith (curry (Data.Bifunctor.second (elemIndex '^') )) [0..] gmap, isJust cidx]

directedPerms :: [(Int, Int)] -> [[(Int, Int)]]
directedPerms xs = map (\l -> head xs : take l xs) [1..(length xs)] ++ directedPerms (tail xs)
