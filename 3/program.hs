#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

-- test_input.txt <- 198
-- input.txt <- 1739283308

import Debug.Trace (traceShow)
import Data.Bits (shift, complement, (.&.))
import Data.Word

main :: IO ()
main = do
  content <- readFile "input.txt"
  let reportValues = lines content
  let transposed = transpose reportValues
  let gammaRate = calculateGamma transposed
  let epsilonRate = clearBitsToWidth (flipBits gammaRate) (reportWidth reportValues)
  let powerConsumption = gammaRate * epsilonRate
  putStrLn (show powerConsumption)

transpose :: [String] -> [String]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

calculateGamma :: [String] -> Int
calculateGamma values =
  foldl (\acc value -> ((shift acc 1)+(mostCommonBit value))) 0 values

mostCommonBit :: String -> Int
mostCommonBit value = if (count '1' value) > (count '0' value) then 1 else 0

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

flipBits :: Int -> Int
flipBits x = fromIntegral (complement (fromIntegral x :: Word))

clearBitsToWidth :: Int -> Int -> Int
clearBitsToWidth number width =
  (.&.) number clrValue
  where
    clrValue = foldl (\acc i -> (shift acc 1) + 1) 0 [1..width]

reportWidth :: [String] -> Int
reportWidth xs = length (head xs)

