{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Map as M
import Debug.Trace

type Days           = Int
type Count          = Int
type Timer          = Int
type FishClusterMap = M.Map Timer Count

split :: Char -> String -> [String]
split _ ""          = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
    in start : split delimiter remain


main :: IO ()
main = do
    contents <- readFile "input"
    let timers = map read . split ',' $ contents

    putStrLn $ "Part 1: " ++ solve1 timers
    putStrLn $ "Part 2: " ++ solve2 timers

    return ()


timersToFishCluster :: [Int] -> FishClusterMap
timersToFishCluster = M.fromListWith (+) . map (, 1)

simulateFishReproduction :: Days -> FishClusterMap -> FishClusterMap
simulateFishReproduction d cm
        | d <= 0    = cm
        | otherwise = simulateFishReproduction (d - 1)
                      . M.insertWith (+) 8 newFishCount
                      . M.insertWith (+) 6 newMothersCount
                      . M.delete (-1)
                      $ newGenM
    where newGenM         = M.mapKeys (subtract 1) cm
          newFishCount    = M.findWithDefault 0 (-1) newGenM
          newMothersCount = M.findWithDefault 0 (-1) newGenM

solve1 :: [Int] -> String
solve1 = show . sum . M.elems . simulateFishReproduction 80 . timersToFishCluster

solve2 :: [Int] -> String
solve2 = show . sum . M.elems . simulateFishReproduction 256 . timersToFishCluster
