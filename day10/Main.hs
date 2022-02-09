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
    putStrLn $ "Part 2: " ++ "to be implemented"
    return ()


data Parser = Parser
    { inputStr :: String
    , stack :: [Char]
    } deriving (Show)

type ParserResult = Either Char Bool

pairs :: [(Char, Char)]
pairs = [ ('[', ']')
        , ('{', '}')
        , ('<', '>')
        , ('(', ')')
        ]


scoreMap :: M.Map Char Int
scoreMap = M.fromList [ (')', 3)
                      , (']', 57)
                      , ('}', 1197)
                      , ('>', 25137)
                      ]

findPairFor :: Char -> (Char, Char)
findPairFor x = case find (uncurry (||) . bimap (== x) (== x)) pairs of
                  Just pair -> pair
                  Nothing -> error $ "Unsupported character" ++ [x]

parse :: Parser -> ParserResult
parse Parser { inputStr = [], stack = [] } = Right True
parse Parser { inputStr = [], stack = (y:ys) } = Right True
parse Parser { inputStr = (x:xs), stack = ss }
    | x == fst pair = handleOpening pair
    | otherwise = handleClosing pair
    where pair = findPairFor x
          handleOpening (_, b) = parse Parser { inputStr = xs, stack = b:ss }
          handleClosing (_, b) = case ss of
            (y:ys) -> if b == y 
                         then parse Parser { inputStr = xs, stack = ys }
                         else Left x
            [] -> Left x

solve1 :: [String] -> String
solve1 = show
    . sum
    . map (flip (M.findWithDefault 0) scoreMap)
    . lefts
    . map runParser
    where runParser s = parse Parser { inputStr = s, stack = [] }
