module Day4 (day4) where

day4 :: IO ()
day4 = do
    contents <- readFile "../input/day4.txt"
    let line_arr = lines contents
    print (part1 line_arr)
    print (part2 line_arr)

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

part2 :: [String] -> Int
part2 chars = length (filter (`isXMAS` chars) [0 .. (length chars * length (head chars) - 1) ])

isXMAS :: Int -> [String] -> Bool
isXMAS idx chars =
    (r_idx /= 0 && r_idx /= length chars - 1)
    && (c_idx /= 0 && c_idx /= length (head chars) - 1)
    && ((chars !! r_idx) !! c_idx == 'A')
    && isMS c1 c2
    && isMS c3 c4
    where
        r_idx = div idx (length (head chars))
        c_idx = mod idx (length (head chars))
        c1 = (chars !! (r_idx - 1)) !! (c_idx - 1)
        c2 = (chars !! (r_idx + 1)) !! (c_idx + 1)
        c3 = (chars !! (r_idx - 1)) !! (c_idx + 1)
        c4 = (chars !! (r_idx + 1)) !! (c_idx - 1)

isMS :: Char -> Char -> Bool
isMS c1 c2 = (c1 /= c2) && (c1 == 'M' || c1 == 'S') && (c2 == 'M' || c2 == 'S')