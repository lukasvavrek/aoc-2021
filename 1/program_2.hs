#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

-- input.txt <- 1797
-- test_input.txt <- 5

main :: IO ()
main = do
  content <- readFile "input.txt"
  let values = map (\line -> read line :: Integer) (lines content)
  let groups = group values
  let increases = countIncreases groups
  putStrLn (show increases)

group :: [Integer] -> [Integer]
group xs
  | length xs >= 3 = [sum (take 3 xs)] ++ group (tail xs)
  | otherwise = []

countIncreases :: [Integer] -> Integer
countIncreases (x:y:[]) = cmp x y
countIncreases (x:y:xs) = (cmp x y) + countIncreases (y:xs)

cmp :: Integer -> Integer -> Integer
cmp x y = if y > x then 1 else 0

