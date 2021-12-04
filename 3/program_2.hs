#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

-- test_input.txt <- 230
-- input.txt <- 4406844

import Debug.Trace (traceShow)
import Data.Bits (shift, complement, (.&.))
import Data.Word
import Data.Char (digitToInt)
import Data.List (foldl')

main :: IO ()
main = do
  content <- readFile "input.txt"
  let reportValues = lines content
  let oxygenGeneratorRating = calculateOxygenGenerator reportValues
  let co2ScrubberRating = calculateCo2ScrubberRating reportValues
  let lifeSupportRating = oxygenGeneratorRating * co2ScrubberRating
  putStrLn (show lifeSupportRating)

calculateOxygenGenerator :: [String] -> Int
calculateOxygenGenerator report = toDec $ progressiveBitFilter report 0 mostCommonBitAtPosition

calculateCo2ScrubberRating :: [String] -> Int
calculateCo2ScrubberRating report = toDec $ progressiveBitFilter report 0 leastCommonBitAtPosition

progressiveBitFilter :: [String] -> Int -> ([String] -> Int -> Char) -> String
progressiveBitFilter report currentPosition bitCalculator
  | length report == 1 = head report
  | otherwise = progressiveBitFilter filtered (currentPosition+1) bitCalculator
  where
    mcb = bitCalculator report currentPosition
    filtered = filterReport report mcb currentPosition

filterReport :: [String] -> Char -> Int -> [String]
filterReport report bit position =
  filter (\row -> row !! position == bit) report

mostCommonBitAtPosition :: [String] -> Int -> Char
mostCommonBitAtPosition report position =
  mostCommonBit (map (\row -> row !! position) report)

mostCommonBit :: String -> Char
mostCommonBit value
  | c1 < c0   = '0'
  | otherwise = '1'
  where
    c1 = count '1' value
    c0 = count '0' value

leastCommonBitAtPosition :: [String] -> Int -> Char
leastCommonBitAtPosition report position =
  leastCommonBit (map (\row -> row !! position) report)

leastCommonBit :: String -> Char
leastCommonBit value
  | c1 < c0   = '1'
  | otherwise = '0'
  where
    c1 = count '1' value
    c0 = count '0' value

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

