module Main where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find)
import Data.Maybe (fromJust)
import Control.Arrow (first)
import Data.Tuple (swap)

main :: IO ()
main = do
    contents <- readFile "input"
    let grid = map (map (read . (:[]))) . lines $ contents

    putStrLn $ "Part 1: " ++ solve1 grid
    putStrLn $ "Part 2: " ++ solve2 grid
    return ()


type EnergyLevel = Int
type Point = (Int, Int)
type OctopusMap = Map Point EnergyLevel
type FlashCount = Int

toCoordsMapHelper :: [(Int, [a])] -> Map Point a -> Map Point a
toCoordsMapHelper [] m             = m
toCoordsMapHelper ((py, xs):xss) m = toCoordsMapHelper xss mappedRow
    where zipped           = zip [0..] xs
          folder a (px, x) = M.insert (px, py) x a
          mappedRow        = foldl folder m zipped

listToCoordsMap :: [[a]] -> Map (Int, Int) a
listToCoordsMap = flip toCoordsMapHelper M.empty . zip [0..]

getPointNeighbors :: Point -> [Point]
getPointNeighbors (x, y) = [ (x - 1, y - 1)
                           , (x - 1, y)
                           , (x - 1, y + 1)
                           , (x, y - 1)
                           , (x, y + 1)
                           , (x + 1, y - 1)
                           , (x + 1, y)
                           , (x + 1, y + 1)
                           ]

simulateRecursive :: Set Point -> OctopusMap -> (OctopusMap, Set Point)
simulateRecursive fs m
    | null readyToFlash = (m, fs)
    | otherwise = let updatedMap = foldr bumpNeighbors drainedMap readyToFlash
                  in simulateRecursive newFs updatedMap
    where readyToFlash      = M.keys . M.filter (> 9) $ m
          drainedMap        = foldr drainEnergy m readyToFlash
          newFs             = foldr S.insert fs readyToFlash
          bumpNeighbors p m = foldr bumpPoint m
                              . filter (`S.notMember` newFs)
                              . getPointNeighbors
                              $ p
          drainEnergy = M.update (\_ -> Just 0)
          bumpPoint   = M.update (Just . (+ 1))

simulate :: Int -> OctopusMap -> (OctopusMap, FlashCount)
simulate 0 m = (m, 0)
simulate n m = (rm, fc + S.size fs)
    where (rm, fc) = simulate (n - 1) cm
          (cm, fs) = simulateRecursive S.empty . M.map (+ 1) $ m

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

solve1 :: [[Int]] -> String
solve1 grid = show
    . snd
    . simulate 100
    $ coordsMap
    where coordsMap = listToCoordsMap grid

solve2 :: [[Int]] -> String
solve2 grid = show
    . fst
    . fromJust
    . find (isFullFlash . snd)
    . scanl (curry (swap . first (fst . simulate 1 . snd))) (0, coordsMap)
    $ [1..]
    where coordsMap   = listToCoordsMap grid
          isFullFlash = all (== 0) . M.elems
