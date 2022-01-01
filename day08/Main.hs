module Main where

import Debug.Trace
import qualified Data.Map as M
import Data.List (union, (\\), intersect, concat, sortOn, find, sort)
import Data.Tuple (swap)
import Data.Ord

-- The solution for part 2 is absolute garbage, it took me 2 days to solve that shit

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

digitSegmentsMap :: M.Map Int String
digitSegmentsMap = M.fromList digitSegmentsList

reverseDigitSegmentsMap :: M.Map String Int
reverseDigitSegmentsMap = M.fromList . map swap $ digitSegmentsList

digitSegmentsCharCountMap :: M.Map Int [String]
digitSegmentsCharCountMap = M.fromListWith (++)
                            . map (\(x, y) -> (length y, [y]))
                            $ digitSegmentsList

data SignalLine = SignalLine { input :: [String]
                             , output :: [String]
                             } deriving (Show)

type PossibilityMap = M.Map Char String

parseSignalLine :: String -> SignalLine
parseSignalLine s = case split '|' s of
                        [i, o] -> SignalLine { input = words i, output = words o }
                        _      -> error ("Parse error on: " ++ s)
    where parts = split '|' s

main :: IO ()
main = do
    contents <- readFile "input"
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

getCombinations :: [String] -> [String]
getCombinations []     = []
getCombinations [x]    = map (:[]) x
getCombinations (s:ss) = foldr folder [] s
    where rest                 = getCombinations ss
          folder x             = (++) (combineWithoutDups x)
          combineWithoutDups c = foldr (\x a -> if c `elem` x then a else (c:x):a) [] rest

lookupWithError :: Ord k => k -> M.Map k a -> a
lookupWithError k m = case M.lookup k m of Just x -> x
                                           Nothing -> error "Key not found"

translateEncoding :: String -> String -> String
translateEncoding key = sort . map (`lookupWithError` translationMap)
    where translationMap = M.fromList . zip ['a'..'g'] $ key


isCombinationValid :: [String] -> String -> Bool
isCombinationValid [] cb       = True
isCombinationValid (ts:tss) cb = M.member translated reverseDigitSegmentsMap 
                                 && isCombinationValid tss cb
    where translated = translateEncoding cb ts

findValidCombination :: [String] -> [String] -> String
findValidCombination tss cbs = case find (isCombinationValid tss) cbs of Just cb -> cb
                                                                         Nothing -> error "Couldn't find combination"

-- The following function is a disaster, I've written it and I have no idea how it works
reducePossibilityMap :: PossibilityMap -> PossibilityMap
reducePossibilityMap m = foldr folder m sortedKeys
    where sortedKeys = map fst . sortOn (Down . length . snd) . M.toList $ m
          folder k a = let segs = M.findWithDefault "" k a
                       in M.insert k segs
                          . M.map (diff segs)
                          . M.delete k
                          $ a
          diff segs x = if null (x \\ segs) && null (segs \\ x) then x else x \\ segs

segsToNumbers :: [String] -> Int
segsToNumbers = foldr folder 0 . reverse
    where folder x a = (+ (a * 10)) . lookupWithError x $ reverseDigitSegmentsMap

solve2 :: [SignalLine] -> String
solve2 []  = "Not found"
solve2 sls = show . sum . map solve $ sls
    where solve sl         = let key = findDecodeKey $ input sl
                                 out = segsToNumbers . map (translateEncoding key) $ output sl
                             in out
          translate key    = translateEncoding key 
          findDecodeKey ss = findValidCombination ss
                             . getCombinations
                             . map snd
                             . sortOn fst
                             . M.toList
                             . reducePossibilityMap
                             $ buildPossibilityMap ss M.empty
