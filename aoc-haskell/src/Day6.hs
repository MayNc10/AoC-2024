module Day6 (day6) where

import Data.Array.Base ((!?), array, assocs)
import Data.Array (Array, Ix)
import Data.Maybe 
import Data.List

data Coord = Coord Int Int deriving (Eq, Ord, Ix, Show)

charsToArray :: [[Char]] -> Array Coord Char
charsToArray chars = 
  array (Coord 0 0, Coord (length chars - 1) (length (head chars) - 1)) 
  (zip  [(Coord r c )| r <- [0..(length chars - 1)], c <- [0..(length (head chars) - 1)]] (concat chars))

day6 = do

    contents <- readFile "../input/day6test.txt"
    let line_arr = lines contents
    let char_array = charsToArray line_arr
    print char_array
    print (part1 char_array)


part1 :: Array Coord Char -> Int
part1 gmap = length (nub path)
    where path = guardPath gmap (guardLoc gmap) (Coord (-1) 0)


guardPath :: Array Coord Char -> Coord -> Coord -> [Coord]
guardPath gmap loc dir | isNothing c = []
                       | c == Just '#' = loc : (guardPath gmap loc (rightTurn dir))
                       | otherwise = loc : (guardPath gmap (loc + dir) dir)
                       where c = gmap !? loc

rightTurn :: Coord -> Coord
rightTurn (Coord vdir hdir) = Coord hdir (vdir * (-1))

guardLoc :: Array Coord Char -> Coord
guardLoc gmap = head [loc | (loc, char) <- assocs gmap, char == '^'] 

