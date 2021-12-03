module Main where

main :: IO ()
main =
    do
        contents <- readFile "input"
        let nums = map read . lines $ contents
        putStrLn $ "Part1: " ++ solve1 nums


getSiblingsList :: [a] -> [(a, a)]
getSiblingsList [] = []
getSiblingsList [x] = []
getSiblingsList (x:y:xs) = (x, y) : getSiblingsList (y:xs)

solve1 :: [Int] -> String
solve1 nums =
        show . length . filter id $ states
    where siblingsList = getSiblingsList nums
          states = map (uncurry (<)) siblingsList
