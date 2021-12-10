{-# LANGUAGE TupleSections #-}
module Main where

import Data.List (groupBy, intercalate, transpose, find)
import Debug.Trace
import Control.Arrow ((&&&), arr)


type Cell = (Int, Bool)

data Puzzle = Puzzle { moves :: [Int]
                     , boards :: [[Cell]]
                     } deriving (Show)

split :: Char -> String -> [String]
split c []  = []
split c str = x : split c rest
    where (x, xs) = break (== c) str
          rest    = if null xs then "" else tail xs

parseBoards :: [String] -> [[Cell]]
parseBoards []    = []
parseBoards lines = currBoard : parseBoards (drop 5 remaining)
    where remaining = dropWhile null lines
          currBoard = map ((, False) . read)
                      . words
                      . unwords
                      . take 5
                      $ remaining

main :: IO ()
main = do
    contents <- readFile "input"
    let input = lines contents
    let moves = map read . split ',' . head $ input
    let boards = parseBoards . drop 2 $ input

    let result1 = solve1 Puzzle { moves = moves, boards = boards }

    putStrLn $ "Part 1: " ++ result1

    return ()

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = uncurry (:) . (arr (take n) &&& arr (groupsOf n . drop n)) $ xs

isWinning :: [Cell] -> Bool
isWinning cells = uncurry (||) (hasMarkedRow rows, hasMarkedRow cols)
    where rows = groupsOf 5 cells
          cols = transpose rows
          hasMarkedRow = any (all snd)

markCell :: Int -> [Cell] -> [Cell]
markCell v [] = []
markCell v ((x, m):xs)
    | v == x    = (x, True):xs
    | otherwise = (x, m):markCell v xs

solve1 :: Puzzle -> String
solve1 Puzzle { moves = [], boards = bs } = "Not found"
solve1 Puzzle { moves = (m:ms), boards = bs }
        | Just board <- find isWinning newBoards = show
                                                   . (* m)
                                                   . sum
                                                   . map fst
                                                   . filter (not . snd)
                                                   $ board
        | otherwise = solve1 Puzzle { moves = ms, boards = newBoards }
    where newBoards = map (markCell m) bs
