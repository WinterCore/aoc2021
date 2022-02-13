module Main where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M

type Polymer = String
type PairRule = (String, String)
type PairRuleMap = Map String String

strToPairRule :: String -> PairRule
strToPairRule = (head &&& last) . words 

main :: IO ()
main = do
    contents <- readFile "exampleinput"
    let input = lines contents
    let polymer = head input
    let rules = map strToPairRule . filter (not . null) . tail $ input


    putStrLn $ "Part 1: " ++ solve1 polymer rules
    putStrLn $ "Part 2: " ++ "to be implemented"


windowOf :: Int -> [a] -> [[a]]
windowOf _ [] = []
windowOf _ [x] = []
windowOf n xs = take n xs : windowOf n (tail xs)
    where (as, bs) = splitAt n xs

simulateStep :: Polymer -> PairRuleMap -> Polymer
    simulateStep ss prm = foldl 
    where pairs  = windowOf 2 ss
          folder a x = 

solve1 :: Polymer -> [PairRule] -> String
solve1 s prs = show
    $ windowOf 2 s
    where rulesMap = M.fromList prs
