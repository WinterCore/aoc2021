module Main where

import Debug.Trace
import qualified Data.Map as M
import Data.List (union, (\\), intersect, sortOn)
import Data.Tuple (swap)
import Data.Ord

split :: Char -> String -> [String]
split _ ""          = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
    in start : split delimiter remain

digitSegmentsList :: [(Int, String)]
digitSegmentsList = [ (0, "abcefg")
                    , (1, "cf")
                    , (2, "acdeg")
                    , (3, "acdfg")
                    , (4, "bcdf")
                    , (5, "abdfg")
                    , (6, "abdefg")
                    , (7, "acf")
                    , (8, "abcdefg")
                    , (9, "abcdfg")
                    ]

digitSegmentsMap :: M.Map Int [Char]
digitSegmentsMap = M.fromList digitSegmentsList

reverseDigitSegmentsMap :: M.Map [Char] Int
reverseDigitSegmentsMap = M.fromList . map swap $ digitSegmentsList

digitSegmentsCharCountMap :: M.Map Int [[Char]]
digitSegmentsCharCountMap = M.fromListWith (++)
                            . map (\(x, y) -> (length y, [y]))
                            $ digitSegmentsList

data SignalLine = SignalLine { input :: [String]
                             , output :: [String]
                             } deriving (Show)

type PossibilityMap = M.Map Char [Char]

parseSignalLine :: String -> SignalLine
parseSignalLine s = case split '|' s of
                        [i, o] -> SignalLine { input = words i, output = words o }
                        _      -> error ("Parse error on: " ++ s)
    where parts = split '|' s

main :: IO ()
main = do
    contents <- readFile "exampleinput"
    let signalLines = map parseSignalLine . lines $ contents

    putStrLn $ "Part 1: " ++ solve1 signalLines
    putStrLn $ "Part 2: " ++ solve2 signalLines

    return ()

solve1 :: [SignalLine] -> String
solve1 = show . sum . map countUniqueSegmentDigits
    where isUniqueSegment = flip elem [2, 3, 4, 7] . length
          countUniqueSegmentDigits SignalLine { output = o } =
              length . filter isUniqueSegment $ o

buildPossibilityMap :: [String] -> PossibilityMap -> PossibilityMap
buildPossibilityMap []     m = m
buildPossibilityMap (s:ss) m = buildPossibilityMap ss updatedMap
    where currSegs    = foldr union []
                        . M.findWithDefault [] (length s)
                        $ digitSegmentsCharCountMap
          updatedMap  = foldr folder m s
          folder k om = case M.lookup k om of
                                Just d  -> M.insert k (d `intersect` currSegs) om
                                Nothing -> M.insert k currSegs om 
          -- debug       = traceShow (s ++ "-" ++ currSegs ++ "--------- after:" ++ show updatedMap)

reducePossibilityMap :: PossibilityMap -> PossibilityMap
reducePossibilityMap m = foldr folder m sorted
    where sorted = traceShowId . sortOn (Down . length . snd) . M.toList $ m
          folder (k, segs) a = M.insert k segs
                               . M.map (\\ segs)
                               . M.delete k
                               $ a

solve2 :: [SignalLine] -> String
solve2 [] = "wtf"
solve2 (SignalLine { input = i, output = o} : ss) = show
                                                    . reducePossibilityMap
                                                    $ buildPossibilityMap i M.empty

-- eafb cagedb ab

-- 0 -> [a, b, c, e, f, g]
-- 1 -> [c, f]
-- 2 -> [a, c, d, e, g]
-- 3 -> [a, c, d, f, g]
-- 4 -> [b, c, d, f]
-- 5 -> [a, b, d, f, g]
-- 6 -> [a, b, d, e, f, g]
-- 7 -> [a, c, f]
-- 8 -> [a, b, c, d, e, f, g]
-- 9 -> [a, b, c, d, f, g]

-- a -> [a, b, c, d, e, f, g]
-- b -> [b, c, d, e, f]
-- c -> [b, c, d, e, f]
-- d -> [b, c, d, e, f]
-- e -> [b, c, d, e, f]
-- f -> [b, c, d, e, f]
-- g -> [a, b, c, d, e, f, g]

-- Steps
-- Find all digits that require the same number of segments
-- Mix them up and remove duplicates
-- Create a map of all the combinations of the letters
-- repeat

-- reduce by starting from the least possibilities and removing them from the others
