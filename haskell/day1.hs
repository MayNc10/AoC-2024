import Data.List
import System.IO
import Control.Monad

main = do
    contents <- readFile "input/day1.txt"
    let line_arr = map words (lines contents)
    let list1 = getList line_arr 0
    let list2 = getList line_arr 1
    print (part1 list1 list2)
    print (part2 list1 list2)

getList :: [[String]] -> Int -> [Int]
getList list idx = sort (map (read . (!! idx)) list)

part1 :: [Int] -> [Int] -> Int
part1 list1 list2 = foldl (\acc item -> acc + abs (snd item - list2 !! fst item) ) 0 (zip [0..] list1)

part2 :: [Int] -> [Int] -> Int
part2 list1 list2 = foldl (\acc item -> acc + item * length (filter (== item) list2) ) 0 list1