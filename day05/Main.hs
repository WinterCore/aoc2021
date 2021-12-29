{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Map as M

type Point    = (Int, Int)
type VentLine = (Point, Point)

split :: Char -> String -> [String]
split _ ""          = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
    in start : split delimiter remain

parseVentLine :: String -> VentLine
parseVentLine s = case words s of
                    [start, _, finish] -> (toPoint start, toPoint finish)
                    _                  -> error "Parse error"
    where toPoint s = case split ',' s of
                        [x, y] -> (read x, read y)
                        _      -> error "Parse error"

main :: IO ()
main = do
    contents <- readFile "input"
    let vents = map parseVentLine . lines $ contents

    putStrLn $ "Part 1: " ++ solve1 vents
    putStrLn $ "Part 2: " ++ solve2 vents

    return ()

range :: Int -> Int -> [Int]
range a b = if a < b then [a..b] else [a, (a - 1)..b]

ventLineToPointsBasic :: VentLine -> [Point]
ventLineToPointsBasic ((x1, y1), (x2, y2))
    | x1 == x2  = zip (repeat x1) (range y1 y2)
    | y1 == y2  = zip (range x1 x2) (repeat y1)
    | otherwise = []

-- The difference between this function and the one above
-- is that this one processes diagonal lines
ventLineToPointsAdvanced :: VentLine -> [Point]
ventLineToPointsAdvanced ((x1, y1), (x2, y2))
    | x1 == x2  = zip (repeat x1) (range y1 y2)
    | y1 == y2  = zip (range x1 x2) (repeat y1)
    | otherwise = zip (range x1 x2) (range y1 y2)

solve1 :: [VentLine] -> String
solve1 vls = show . length . M.filter (>= 2) $ graphMap
    where points   = concatMap ventLineToPointsBasic vls
          graphMap = M.fromListWith (+) . map (, 1) $ points

solve2 :: [VentLine] -> String
solve2 vls =  show . length . M.filter (>= 2) $ graphMap
    where points   = concatMap ventLineToPointsAdvanced vls
          graphMap = M.fromListWith (+) . map (, 1) $ points

