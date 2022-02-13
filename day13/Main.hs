module Main where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow ((&&&), first, second)

type Point = (Int, Int)
type DotsSet = Set Point

data FoldInstruction = FoldX Int | FoldY Int deriving (Show)

getFoldVal :: FoldInstruction -> Int
getFoldVal (FoldX n) = n
getFoldVal (FoldY n) = n

split :: Char -> String -> [String]
split _ ""          = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain)   = span (== delimiter) rest
    in start : split delimiter remain

strToCoord :: String -> Point
strToCoord s
    | length d == 2 = (read (head d), read (last d))
    | otherwise     = error "Parse error"
    where d = split ',' s

strToFoldInstruction :: String -> FoldInstruction
strToFoldInstruction s =
    case map toLower (head d) of
        "x" -> FoldX . read . last $ d
        "y" -> FoldY . read . last $ d
        _   -> error "Parse error"
    where d = split '=' . last . words $ s

main :: IO ()
main = do
    contents <- readFile "input"
    let input = lines contents
    let coords = map strToCoord . takeWhile (not . null) $ input
    let instructions = map strToFoldInstruction
            . filter (not . null)
            . drop (length coords)
            $ input

    putStrLn $ "Part 1: " ++ solve1 coords instructions
    putStrLn $ "Part 2: " ++ solve2 coords instructions

    return ()


fold :: FoldInstruction -> DotsSet -> DotsSet
fold ins ds = foldr folder a (S.toList b)
    where (a, b) = (S.filter ((< foldVal) . getAxis) &&& S.filter ((> foldVal) . getAxis)) ds
          folder p = S.insert (replaceAxis (foldVal * 2 -) p)
          foldVal = getFoldVal ins
          (getAxis, replaceAxis) = case ins of
                                      FoldX _ -> (fst, first)
                                      FoldY _ -> (snd, second)

solve1 :: [Point] -> [FoldInstruction] -> String
solve1 coords = show
    . S.size
    . foldr fold coordsSet
    . reverse
    . take 1
    where coordsSet = S.fromList coords

solve2 :: [Point] -> [FoldInstruction] -> String
solve2 coords = 
    ("\n" ++) . showGrid (50, 10)
    . foldr fold coordsSet
    . reverse
    where coordsSet = S.fromList coords

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
    where (as, bs) = splitAt n xs

showGrid :: (Int, Int) -> DotsSet -> String
showGrid (mx, my) ds = intercalate "\n" . splitEvery (mx * 2) . map getCell $ grid
    where grid = [(x, y) | y <- [(-my)..(my - 1)], x <- [(-mx)..(mx - 1)]]
          getCell p = if S.member p ds
                         then '#'
                         else '.'
