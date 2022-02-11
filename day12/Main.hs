module Main where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow (second, (&&&))
import Data.Tuple (swap)
import Data.Char

split :: Char -> String -> [String]
split _ ""          = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain)   = span (== delimiter) rest
    in start : split delimiter remain

toPair :: Show a => [a] -> Maybe (a, a)
toPair [x, y] = Just (x, y)
toPair xs     = Nothing

main :: IO ()
main = do
    contents <- readFile "input"
    let edges = mapMaybe (toPair . split '-') . lines $ contents

    putStrLn $ "Part 1: " ++ solve1 edges
    putStrLn $ "Part 2: " ++ "to be implemented"
    return ()

type Edge = (String, String)
type EdgeMap = Map String [String]

edgeListToEdgeMap :: [Edge] -> EdgeMap
edgeListToEdgeMap =
    M.fromListWith (++)
    . map (second (:[]))
    . uncurry (++)
    . (id &&& map swap)


findPossiblePaths :: EdgeMap -> Set String -> String -> [[String]]
findPossiblePaths m vs start
    | start == "end"    = [[start]]
    | S.member start vs = [[]]
    | otherwise         = map ([start] ++) rest
    where nodes     = M.findWithDefault [] start m
          rest      = concatMap (findPossiblePaths m updatedVs) nodes
          updatedVs = if all isLower start then S.insert start vs else vs

solve1 :: [Edge] -> String
solve1 xs = show
    . length
    . filter ((== "end") . last)
    . findPossiblePaths em S.empty
    $ "start"
    where em = edgeListToEdgeMap xs
