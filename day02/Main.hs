module Main where

import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

data Command = Forward Int | Down Int | Up Int deriving Show

strToCommand :: String -> Maybe Command
strToCommand s = toCommand =<< parts
    where parts = case words s of [x, y] -> Just (x, y)
                                  xs     -> Nothing
          toCommand (x, y)
            | x == "forward" = Just . Forward $ read y
            | x == "down" = Just . Down $ read y
            | x == "up" = Just . Up $ read y
            | otherwise = Nothing
          

main :: IO ()
main = do
    contents <- readFile "input"
    let commands = mapMaybe strToCommand (lines contents)
    putStrLn $ "Part 1: " ++ solve1 commands
    return ()

solve1 :: [Command] -> String
solve1 cmds = show $ verticalPos * horizontalPos
    where
        getHorizontalValue c = case c of (Forward n) -> n
                                         c           -> 0

        getVerticalValue c = case c of (Down n) -> n
                                       (Up n)   -> -n
                                       c        -> 0
        verticalPos = sum . map getVerticalValue $ cmds
        horizontalPos = sum . map getHorizontalValue $ cmds
