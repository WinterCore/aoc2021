module Main where

import Data.Bits
import Data.List (transpose)
import Data.Char (digitToInt)

main :: IO ()
main = do
    contents <- readFile "input"
    let binaryNums = lines contents
    putStrLn $ "Part 1: " ++ solve1 binaryNums
    putStrLn $ "Part 2: " ++ solve2 binaryNums
    return ()

binToInt :: [Int] -> Int
binToInt = foldl (\r x -> shift r 1 .|. x) 0

binToBitStates :: [Char] -> (Int, Int)
binToBitStates bin = (on bin, off bin)
    where
        on = length . filter (== '1')
        off = length . filter (== '0')

toProminent :: (Int, Int) -> (Int, Int)
toProminent (a, b) = if a > b then (1, 0) else (0, 1)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

solve1 :: [String] -> String
solve1 binaryNums = show $ gamma * epsilon
    where
        columns = transpose binaryNums
        states = map binToBitStates columns
        gamma = binToInt . map (fst . toProminent) $ states
        epsilon = binToInt . map (snd . toProminent) $ states


shake :: ((Int, Int) -> Int) -> Int -> [String] -> [String]
shake predicate i bins =
    if i == bitLength || length bins == 1 then bins
    else shake predicate (i + 1)
        . filterBitAtIndex (head . show $ prominent) i
        $ bins
    where
        filterBitAtIndex b i = filter ((== b) . (!! i))
        bitLength            = length . head $ bins
        prominent            = predicate
                               . binToBitStates
                               . (!! i)
                               . transpose
                               $ bins

solve2 :: [String] -> String
solve2 bins = show . uncurry (*) $ (oxygen bins, co2 bins)
    where
        oxygen              = getRating oxygenPredicate
        co2                 = getRating co2Predicate
        getRating predicate = binToInt
                              . map digitToInt
                              . head
                              . shake predicate 0
        oxygenPredicate (a, b)
            | a == b    = 1
            | otherwise = fst . toProminent $ (a, b)
        co2Predicate (a, b)
            | a == b    = 0
            | otherwise = snd . toProminent $ (a, b)
        
