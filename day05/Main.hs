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

    return ()

range :: Int -> Int -> [Int]
range a b = if a < b then [a..b] else [a, (a - 1)..b]

ventLineToPoints :: VentLine -> [Point]
ventLineToPoints ((x1, y1), (x2, y2))
    | x1 == x2  = zip (repeat x1) (range y1 y2)
    | y1 == y2  = zip (range x1 x2) (repeat y1)
    | otherwise = [] -- We don't handle diagonals for now

solve1 :: [VentLine] -> String
solve1 vls = show . length . M.filter (>= 2) $ graphMap
    where points   = concatMap ventLineToPoints vls
          graphMap = M.fromListWith (+) . map (, 1) $ points
