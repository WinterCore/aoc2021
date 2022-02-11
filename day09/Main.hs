module Main where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import Data.List (nubBy, sortOn)


type Point = (Int, Int)

data BasinWalker = BasinWalker { visited :: Set Point
                               , remaining :: [Point]
                               , basin :: [Point]
                               } deriving (Show)

liftSndMaybe :: (a, Maybe b) -> Maybe (a, b)
liftSndMaybe (x, y) =
    case y of Just v  -> Just (x, v)
              Nothing -> Nothing

main :: IO ()
main = do
    contents <- readFile "input"
    let grid = map (map (read . (:[]))) . lines $ contents :: [[Int]]


    putStrLn $ "Part 1: " ++ solve1 grid
    putStrLn $ "Part 2: " ++ solve2 grid
    return ()

toCoordsMapHelper :: [(Int, [a])] -> Map (Int, Int) a -> Map (Int, Int) a
toCoordsMapHelper [] m             = m
toCoordsMapHelper ((py, xs):xss) m = toCoordsMapHelper xss mappedRow
    where zipped           = zip [0..] xs
          folder a (px, x) = M.insert (px, py) x a
          mappedRow        = foldl folder m zipped

listToCoordsMap :: [[a]] -> Map (Int, Int) a
listToCoordsMap = flip toCoordsMapHelper M.empty . zip [0..]

getAdjacentCells :: (Int, Int) -> Map (Int, Int) a -> [((Int, Int), a)]
getAdjacentCells (x, y) m = getCells adjPoints
    where adjPoints           = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
          getCells            = mapMaybe (liftSndMaybe . (id &&& (`M.lookup` m)))


solve1 :: [[Int]] -> String
solve1 xss = show
             . sum
             . map ((+1) . snd)
             . M.toList
             . M.filterWithKey filterer
             $ coordsMap
    where coordsMap  = listToCoordsMap xss
          filterer k v = all ((> v) . snd) . getAdjacentCells k $ coordsMap


toCoordsMap :: [((Int, Int), a)] -> Map (Int, Int) a
toCoordsMap = foldr folder M.empty
    where folder (k, v) = M.insert k v


findBasin :: Map Point Int -> Set Point -> Point -> Maybe BasinWalker
findBasin cm vs p =
    case M.lookup p cm of
        Just v  -> if v < 9 then Just (walkBasin BasinWalker { visited = S.insert p vs, remaining = [p], basin = [] } cm) else Nothing
        Nothing -> Nothing

walkBasin :: BasinWalker -> Map Point Int -> BasinWalker
walkBasin bw@BasinWalker { remaining = [] } cm  = bw
walkBasin BasinWalker { visited    = vs
                      , remaining  = ((x, y):rs)
                      , basin      = bs } cm = walkBasin (BasinWalker { visited = newVs, remaining = rs ++ adjCells, basin = (x, y):bs }) cm
    where newVs     = foldr S.insert vs adjCells
          adjCells  = map fst
                      . filter (uncurry (&&) . ((< 9) . snd &&& (`S.notMember` vs) . fst))
                      . mapMaybe (liftSndMaybe . (id &&& (`M.lookup` cm)))
                      $ [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

findAllBasins :: Map Point Int -> [[Point]]
findAllBasins cm = snd . foldr folder (S.empty, []) $ points
    where points          = M.keys cm
          folder p (vs, bss) = case findBasin cm vs p of
                                    Just BasinWalker { visited = nvs, basin = bs } -> (nvs, bs:bss)
                                    Nothing -> (vs, bss)

solve2 :: [[Int]] -> String
solve2 xss = show
             . product
             . take 3
             . sortOn negate
             . map length
             . findAllBasins
             $ coordsMap
    where coordsMap = listToCoordsMap xss
