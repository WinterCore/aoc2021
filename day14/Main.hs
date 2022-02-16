{-# LANGUAGE TupleSections #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow ((&&&))

type Polymer = String
type PairRule = (String, String)
type PairRuleMap = Map String String

strToPairRule :: String -> PairRule
strToPairRule = (head &&& last) . words 

main :: IO ()
main = do
    contents <- readFile "input"
    let input = lines contents
    let polymer = head input
    let rules = map strToPairRule . filter (not . null) . tail $ input


    putStrLn $ "Part 1: " ++ solve1 rules polymer
    putStrLn $ "Part 2: " ++ "to be implemented"


windowOf :: Int -> [a] -> [[a]]
windowOf _ [] = []
windowOf _ [x] = []
windowOf n xs = take n xs : windowOf n (tail xs)
    where (as, bs) = splitAt n xs

simulateStep :: PairRuleMap -> Polymer -> Polymer
simulateStep prm ss = foldl folder [head ss] pairs
    where pairs  = windowOf 2 ss
          folder a x = case M.lookup x prm of
                         Just val -> a ++ val ++ [last x]
                         Nothing  -> a ++ [last x]

solve1 :: [PairRule] -> Polymer -> String
solve1 prs = show
    . uncurry (-)
    . (maximum &&& minimum)
    . M.elems
    . M.fromListWith (+)
    . map (, 1)
    . last
    . take (10 -- The number of steps
            + 1)
    . iterate (simulateStep rulesMap)
    where rulesMap = M.fromList prs
