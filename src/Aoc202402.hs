{-# LANGUAGE TypeApplications #-}

module Aoc202402 (solve) where

allAdjacents :: (b -> b -> Bool) -> [b] -> Bool
allAdjacents f xs = all (uncurry f) $ zip xs (tail xs)

inRange :: (Ord a) => a -> a -> a -> Bool
inRange x lb ub = lb <= x && x <= ub

isSafeReport :: [Int] -> Bool
isSafeReport reports =
  let increasing = allAdjacents (<) reports
      decreasing = allAdjacents (>) reports
   in (increasing || decreasing) && allAdjacents (\x y -> inRange (abs $ x - y) 1 3) reports

part1Sol :: [[Int]] -> Int
part1Sol = foldr (\report acc -> if isSafeReport report then acc + 1 else acc) 0

solve :: IO ()
solve = do
  content <- readFile "Aoc202402.txt"
  let reports = map (map (read @Int) . words) $ lines content
  putStrLn $ "Part 1 answer: " ++ show (part1Sol reports)
