module Main where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow (second, (&&&))
import Data.Tuple (swap)
import Data.Char
import Data.List

-- Part 2 needs optimization, it takes around 3-4 seconds to run

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
    putStrLn $ "Part 2: " ++ solve2 edges
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

findPossiblePaths2 :: EdgeMap -> Map String Int -> String -> String -> [[String]]
findPossiblePaths2 m vm x2 "end" = [["end"]]
findPossiblePaths2 m vm x2 start =
    case M.lookup start vm of
        Just x  -> if x < 1 || (start == x2 && x < 2) then combs else [[]]
        Nothing -> combs
    where combs     = map ([start] ++) rest
          nodes     = M.findWithDefault [] start m
          rest      = concatMap (findPossiblePaths2 m updatedVm x2) nodes
          updatedVm = if all isLower start
                      then M.alter vmUpdater start vm
                      else vm
          vmUpdater x = case x of
                          Just y  -> Just (y + 1)
                          Nothing -> Just 1

solve2 :: [Edge] -> String
solve2 xs = show
    . S.size
    -- Move the data into a set to remove dups cuz nub is so fucking slow
    . S.fromList
    . filter ((== "end") . last)
    . concatMap (flip (findPossiblePaths2 em M.empty) "start")
    $ small
    where em = edgeListToEdgeMap xs
          small = filter (uncurry (&&) . (all isLower &&& (`notElem` ["start", "end"]))) . M.keys $ em
