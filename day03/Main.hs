module Main where

import Data.List (transpose)
import Data.Bits

main :: IO ()
main = do
    contents <- readFile "input"
    let binaryNums = lines contents
    putStrLn $ "Part 1: " ++ solve1 binaryNums
    print $ binToInt [1, 0, 1, 1, 0]
    return ()

binToInt :: [Int] -> Int
binToInt = foldl (\r x -> shift r 1 .|. x) 0

binToBitStates :: [Char] -> (Int, Int)
binToBitStates bin = (on bin, off bin)
    where
        on = length . filter (== '0')
        off = length . filter (== '1')

solve1 :: [String] -> String
solve1 binaryNums = show $ gamma * epsilon
    where
        columns = transpose binaryNums
        states = map binToBitStates columns
        toRatio (a, b) = if a > b then (1, 0) else (0, 1)
        gamma = binToInt . map (fst . toRatio) $ states
        epsilon = binToInt . map (snd . toRatio) $ states
