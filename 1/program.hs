#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

-- test_input.txt <- 7
-- input.txt <- 1766

main :: IO ()
main = do
  content <- readFile "input.txt"
  let values = map (\line -> read line :: Integer) (lines content)
  let increases = countIncreases values
  putStrLn (show increases)

countIncreases :: [Integer] -> Integer
countIncreases (x:y:[]) = cmp x y
countIncreases (x:y:xs) = (cmp x y) + countIncreases (y:xs)

cmp :: Integer -> Integer -> Integer
cmp x y = if y > x then 1 else 0

