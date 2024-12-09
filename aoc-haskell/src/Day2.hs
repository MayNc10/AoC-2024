module Day2 (day2) where

day2 :: IO ()
day2 = do
    contents <- readFile "../input/day2.txt"
    let line_arr = map (map read . words) (lines contents)
    print (part1 line_arr)
    print (part2 line_arr)

part1 :: [[Int]] -> Int
part1 line_arr = length (filter id (map isSafe line_arr))

part2 :: [[Int]] -> Int
part2 line_arr = length (filter id (map isSafeDamp line_arr))

isSafeDamp :: [Int] -> Bool
isSafeDamp list = foldl (\acc combo -> acc || isSafe combo) False (map (`deleteAt` list) [0.. (length list)])

isSafe :: [Int] -> Bool
isSafe list = foldl (\acc item -> acc && isItemSafe item (pairDiff (head windowList)) ) True windowList
        where windowList = zip list (tail list)

isItemSafe :: (Int, Int) -> Int -> Bool
isItemSafe pair first = abs (pairDiff pair) >= 1 && abs (pairDiff pair) <= 3 && signum (pairDiff pair) == signum first

pairDiff :: (Int, Int) -> Int
pairDiff = uncurry (-)

deleteAt :: Int -> [Int] -> [Int]
deleteAt idx xs | not (null rgt) = lft ++ tail rgt
                | otherwise = lft
  where (lft, rgt) = splitAt idx xs