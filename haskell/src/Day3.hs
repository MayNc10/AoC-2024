module Day3 (day3) where

import Data.Array
import Data.List
import Data.Maybe
import Debug.Trace
import System.IO
import Control.Monad
import Text.Regex.Base
import Text.Regex.PCRE ((=~))

day3 = do
    contents <- readFile "input/day3.txt"
    print (part2 contents)

part1 :: String -> Int
part1 instructions = score (elems matches)
    where matches = getAllTextMatches (instructions =~ regex) :: Array Int (MatchText String)

part2 :: String -> Int
part2 instructions =
    sum ( map (\item -> (read (fst item) :: Int) * read (snd item) :: Int) (doFilter stringPairList))
    where
        stringPairList = map ( matchListToStrPair . matchListToStrings . elems ) ( elems matches)
        matches = getAllTextMatches (instructions =~ regexDoDont) :: Array Int (MatchText String)


doFilter :: [(String, String)] -> [(String, String)]
doFilter list = map snd (
    filter (\item -> isMul (fst (snd item))
        && ( isNothing (mostRecent (subListRev item list)) || fst (list !! (fst item - fromJust (mostRecent (subListRev item list) ) - 1)  ) == "do()"))
    (zip [0..] list))

subListRev :: (Int, (String, String)) -> [(String, String)] -> [(String, String)]
subListRev item list = reverse (take (fst item) list)

mostRecent :: [(String, String)] -> Maybe Int
mostRecent = findIndex (not . isMul . fst)

score :: [MatchText String] -> Int
score list = sum (map (uncurry (*) . matchToNums ) list)

matchToNums :: MatchText String -> (Int, Int)
matchToNums arr = (read (fst (elems arr !! 1)), read (fst (elems arr !! 2)))

matchListToStrings :: [(String, (MatchOffset, MatchLength))] -> [String]
matchListToStrings = map fst

matchListToStrPair :: [String] -> (String, String)
matchListToStrPair list | isMul (head list) = (list !! 2, list !! 3)
                        | otherwise = (list !! 1, list !! 2)

isMul :: String -> Bool
isMul s = s /= "don't()" && s /= "do()"

regex :: String
regex = "mul\\(([0-9]{1,3})?,([0-9]{1,3})?\\)"

regexDoDont :: String
regexDoDont = "(mul\\(([0-9]{1,3})?,([0-9]{1,3})?\\)|do(n't)?\\(\\))"