module Main where

import Data.Either
import Data.List
import Data.Bifunctor (bimap)
import Control.Arrow ((&&&))
import qualified Data.Map as M

main :: IO ()
main = do
    contents <- readFile "input"
    let chunks = lines contents

    putStrLn $ "Part 1: " ++ solve1 chunks
    putStrLn $ "Part 2: " ++ solve2 chunks
    return ()


data Parser = Parser
    { inputStr :: String
    , stack :: [Char]
    } deriving (Show)

data ParseError = Incomplete [Char] | Unexpected Char
type ParserResult = Either ParseError Bool

catIncomplete :: [ParseError] -> [[Char]]
catIncomplete []     = []
catIncomplete (x:xs) = let rs = catIncomplete xs in
                       case x of
                         Incomplete cs -> cs:rs
                         _             -> rs

catUnexpected :: [ParseError] -> [Char]
catUnexpected []     = []
catUnexpected (x:xs) = let rs = catUnexpected xs in
                       case x of
                         Unexpected c -> c:rs
                         _            -> rs


pairs :: [(Char, Char)]
pairs = [ ('[', ']')
        , ('{', '}')
        , ('<', '>')
        , ('(', ')')
        ]


part1ScoreMap :: M.Map Char Int
part1ScoreMap = M.fromList [ (')', 3)
                           , (']', 57)
                           , ('}', 1197)
                           , ('>', 25137)
                           ]

part2ScoreMap :: M.Map Char Int
part2ScoreMap = M.fromList [ (')', 1)
                           , (']', 2)
                           , ('}', 3)
                           , ('>', 4)
                           ]

findPairFor :: Char -> (Char, Char)
findPairFor x = case find (uncurry (||) . bimap (== x) (== x)) pairs of
                  Just pair -> pair
                  Nothing -> error $ "Unsupported character" ++ [x]

parse :: Parser -> ParserResult
parse Parser { inputStr = [], stack = [] } = Right True
parse Parser { inputStr = [], stack = ss } = Left (Incomplete ss)
parse Parser { inputStr = (x:xs), stack = ss }
    | x == fst pair = handleOpening pair
    | otherwise = handleClosing pair
    where pair = findPairFor x
          handleOpening (_, b) = parse Parser { inputStr = xs, stack = b:ss }
          handleClosing (_, b) = case ss of
            (y:ys) -> if b == y
                         then parse Parser { inputStr = xs, stack = ys }
                         else Left (Unexpected x)
            [] -> Left (Unexpected x)

solve1 :: [String] -> String
solve1 = show
    . sum
    . map (flip (M.findWithDefault 0) part1ScoreMap)
    . catUnexpected
    . lefts
    . map runParser
    where runParser s = parse Parser { inputStr = s, stack = [] }

getMiddle :: [a] -> a
getMiddle [] = error "Empty list"
getMiddle xs = xs !! half
    where half = div (length xs) 2

solve2 :: [String] -> String
solve2 = show
    . getMiddle
    . sort
    . map getScore
    . catIncomplete
    . lefts
    . map runParser
    where runParser s = parse Parser { inputStr = s, stack = [] }
          getScore = foldl (\a x -> a * 5 + M.findWithDefault 0 x part2ScoreMap) 0
