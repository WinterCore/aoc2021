{-# LANGUAGE TupleSections #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Data.List (nub)

type PairRule = (String, String)
type PairRuleMap = Map String String
type PolymerPairMap = Map String Int
type LetterCountMap = Map Char Int

strToPairRule :: String -> PairRule
strToPairRule = (head &&& last) . words

main :: IO ()
main = do
    contents <- readFile "input"
    let input = lines contents
    let polymer = head input
    let rules = map strToPairRule . filter (not . null) . tail $ input


    putStrLn $ "Part 1: " ++ solve1 rules polymer
    putStrLn $ "Part 2: " ++ solve2 rules polymer


windowOf :: Int -> [a] -> [[a]]
windowOf _ [] = []
windowOf _ [x] = []
windowOf n xs = take n xs : windowOf n (tail xs)
    where (as, bs) = splitAt n xs

processPair :: PairRuleMap -> String -> Int -> PolymerPairMap -> PolymerPairMap
processPair prm p n ppm =
    case M.lookup p prm of
        Just x  ->
            M.alter (alterer n) (x ++ [last p])
            . M.alter (alterer n) (head p : x)
            $ ppm
        Nothing -> ppm
    where alterer v ev = case ev of
                          Just x  -> Just (x + v)
                          Nothing -> Just v


simulateStep :: PairRuleMap -> PolymerPairMap -> PolymerPairMap
simulateStep prm = M.foldrWithKey (processPair prm) M.empty

mapMaybeWithDefault :: b -> (a -> b) -> Maybe a -> Maybe b
mapMaybeWithDefault d fn m = case m of
                               Just x -> Just (fn x)
                               Nothing -> Just d

countLetters :: String -> PolymerPairMap -> LetterCountMap
countLetters p ppm =
    M.alter (mapMaybeWithDefault 1 (+1)) (head p)
    . M.fromList
    . map (id &&& getCount)
    $ letters
    where letters    = nub . concat . M.keys $ ppm
          entries    = M.toList ppm
          getCount c = sum . map snd . filter ((c ==) . last . fst) $ entries

runPairInsertion :: Int -> [PairRule] -> String -> PolymerPairMap
runPairInsertion n prs = 
    last
    . take (n -- The number of steps
            + 1)
    . iterate (simulateStep rm)
    . M.fromListWith (+)
    . map (, 1)
    . windowOf 2
    where rm  = M.fromList prs

getLetterCountDiff :: LetterCountMap -> Int
getLetterCountDiff = 
    uncurry (-)
    . (maximum &&& minimum)
    . map snd
    . M.toList

solve1 :: [PairRule] -> String -> String
solve1 prs p = show
    . getLetterCountDiff
    . countLetters p
    . runPairInsertion 10 prs
    $ p

solve2 :: [PairRule] -> String -> String
solve2 prs p = show
    . getLetterCountDiff
    . countLetters p
    . runPairInsertion 40 prs
    $ p


