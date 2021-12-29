module Main where

import Debug.Trace

split :: Char -> String -> [String]
split _ ""          = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
    in start : split delimiter remain

data SignalLine = SignalLine { input :: [String]
                             , output :: [String]
                             } deriving (Show)

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

    return ()

solve1 :: [SignalLine] -> String
solve1 = show . sum . map countUniqueSegmentDigits
    where isUniqueSegment = flip elem [2, 3, 4, 7] . length
          countUniqueSegmentDigits SignalLine { output = o } =
              length . filter isUniqueSegment $ o

