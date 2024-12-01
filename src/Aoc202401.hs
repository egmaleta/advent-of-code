module Aoc202401 (solve) where

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let (ys, zs) = splitAt (length xs `div` 2) xs
   in merge (mergeSort ys) (mergeSort zs)

mySplit :: [a] -> ([a], [a])
mySplit [] = ([], [])
mySplit [_] = error "list must be of even length"
mySplit (x : y : zs) = let (xs, ys) = mySplit zs in (x : xs, y : ys)

-- lists must be sorted and of same size
part1Sol :: Num a => [a] -> [a] -> a
part1Sol xs ys = sum $ zipWith (\x y -> abs $ x - y) xs ys

-- list must be sorted
part2Sol :: [Int] -> [Int] -> Int -> Int
part2Sol _ [] acc = acc
part2Sol [] _ acc = acc
part2Sol (x : xs) ys acc =
  let ys' = dropWhile (< x) ys
      (eqX, ys'') = span (== x) ys'
      count = x * length eqX
  in part2Sol xs ys'' (acc + count)

solve :: IO ()
solve = do
  content <- readFile "Aoc202401.txt"
  let strNumbers = words content
  let numbers = map read strNumbers :: [Int]
  let (xs, ys) = mySplit numbers
  let xs' = mergeSort xs
  let ys' = mergeSort ys
  putStrLn $ "Part 1 answer: " ++ show (part1Sol xs' ys')
  putStrLn $ "Part 2 answer: " ++ show (part2Sol xs' ys' 0)
  return ()
