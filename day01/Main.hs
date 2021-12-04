module Main where

main :: IO ()
main = do
    contents <- readFile "input"
    let nums = map read . lines $ contents

    putStrLn $ "Part 1: " ++ solve1 nums
    putStrLn $ "Part 2: " ++ solve2 nums


getWindowList :: Int -> [a] -> [[a]]
getWindowList n x
    | length x < n = []
    | otherwise    = currWindow : getWindowList n (tail x)
    where
        currWindow = take n x

solve1 :: [Int] -> String
solve1 nums = show . length . filter id $ states
    where siblingsList = getWindowList 2 nums
          diffs        = map (foldl1 (-)) siblingsList
          states       = map (< 0) diffs

solve2 :: [Int] -> String
solve2 nums = show . length . filter id $ states
    where windowsList     = getWindowList 3 nums
          sumsOfWindows   = map sum windowsList
          sumsSiblingList = getWindowList 2 sumsOfWindows
          diffs           = map (foldl1 (-)) sumsSiblingList
          states          = map (< 0) diffs

