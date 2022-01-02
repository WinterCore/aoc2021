module Main where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "input"
    let grid = map (map (read . (:[]))) . lines $ contents :: [[Int]]


    putStrLn $ "Part 1: " ++ solve1 grid
    -- putStrLn $ "Part 2: " ++ solve2 signalLines
    return ()

toCoordsMapHelper :: [(Int, [a])] -> M.Map (Int, Int) a -> M.Map (Int, Int) a
toCoordsMapHelper [] m             = m
toCoordsMapHelper ((py, xs):xss) m = toCoordsMapHelper xss mappedRow
    where zipped           = zip [0..] xs
          folder a (px, x) = M.insert (px, py) x a
          mappedRow        = foldl folder m zipped

toCoordsMap :: [[a]] -> M.Map (Int, Int) a
toCoordsMap = flip toCoordsMapHelper M.empty . zip [0..]

getAdjacentCells :: (Int, Int) -> M.Map (Int, Int) a -> [a]
getAdjacentCells (x, y) m = adjCells
    where adjPoints = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
          adjCells  = mapMaybe (`M.lookup` m) adjPoints

solve1 :: [[Int]] -> String
solve1 xss = show
             . sum
             . map ((+1) . snd)
             . M.toList
             . M.filterWithKey filterer
             $ coordsMap
    where coordsMap  = toCoordsMap xss
          filterer k v = all (> v) . getAdjacentCells k $ coordsMap
