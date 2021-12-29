module Main where


split :: Char -> String -> [String]
split _ ""          = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
    in start : split delimiter remain

type Position = Int

main :: IO ()
main = do
    contents <- readFile "input"
    let positions = map read . split ',' $ contents

    putStrLn $ "Part 1: " ++ solve1 positions
    putStrLn $ "Part 2: " ++ solve2 positions

    return ()


getCost :: Position -> [Position] -> Int
getCost s = sum . map getDistance
    where getDistance = abs . (-) s

distanceSum :: Num b => Position -> b
distanceSum x = let xf = fromIntegral x
                in  fromIntegral . truncate $ (xf / 2 * (2 + (xf - 1)))

getCostAdvanced :: Position -> [Position] -> Int
getCostAdvanced s = sum . map getDistance
    where getDistance   = distanceSum . abs . (-) s

getBounds :: Ord a => [a] -> (a, a)
getBounds xs = (minimum xs, maximum xs)

solve1 :: [Position] -> String
solve1 ps = show . minimum . map (`getCost` ps) $ ps

solve2 :: [Position] -> String
solve2 ps = show . minimum . map (`getCostAdvanced` ps) $ [min..max]
    where (min, max) = getBounds ps
