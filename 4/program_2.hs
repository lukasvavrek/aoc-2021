#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

-- test_input.txt <- 1924
-- input.txt <- 19012

import Debug.Trace (traceShow)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let numbers = parseNumbers $ head $ lines content
  let boardsContent = tail $ lines content
  let boards = parseBoards $ boardsContent
  let winningBoard = solveBingo' boards numbers
  let lastMarkedNumber = findLastMarked winningBoard numbers
  let score = calculateScore winningBoard
  let finalScore = lastMarkedNumber * score
  putStrLn (show finalScore)

parseNumbers :: String -> [Int]
parseNumbers input =
  map (\value -> read value :: Int) values
  where
    values = words [if c == ',' then ' ' else c | c <- input]

parseBoards :: [String] -> [Board]
parseBoards inputLines =
  map (\chunk -> parseBoard chunk) (chunksOf 5 validLines)
  where
    validLines = filter (\line -> line /= "") inputLines

parseBoard :: [String] -> Board
parseBoard inputLines = map parseBoardLine inputLines

parseBoardLine :: String -> [Field]
parseBoardLine line = map (\value -> Free (read value :: Int)) (words line)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

markBoards:: [Board] -> Int -> [Board]
markBoards boards number =
  map (\board -> markBoard board number) boards

markBoard:: Board -> Int -> Board
markBoard board number = map (\row -> markRow row number) board

markRow :: [Field] -> Int -> [Field]
markRow row number =  map (\field -> markField field number) row

markField :: Field -> Int -> Field
markField (Free value) number
  | value == number = Marked value
  | otherwise = Free value
markField field number = field

solveBingo' :: [Board] -> [Int] -> Board
solveBingo' boards numbers = head $ foldl markBoardsIfNotSolved boards numbers
  where
    markBoardsIfNotSolved boards number =
      if length boards == 1 && isSolved (boards !! 0)
      then boards
      else markBoards (filter (not . isSolved) boards) number

solveBingo :: Board -> [Int] -> Board
solveBingo board numbers = foldl markBoardIfNotSolved board numbers
  where
    markBoardIfNotSolved board number =
      if isSolved board
      then board
      else markBoard board number

isAnySolved :: [Board] -> Bool
isAnySolved boards =
  foldl (\acc board -> acc || isSolved board) False boards

isSolved :: Board -> Bool
isSolved board =
  anyRowSolved board || anyRowSolved (transpose board)

anyRowSolved :: Board -> Bool
anyRowSolved rows = foldl (\acc row -> acc || (all isMarked row)) False rows

isMarked :: Field -> Bool
isMarked (Marked v) = True
isMarked (Free v) = False

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

calculateScore :: Board -> Int
calculateScore board =
  foldl (\acc field -> acc + (scoreValue field)) 0 (concat board)
  where
    scoreValue (Free value) = value
    scoreValue (Marked _) = 0

findLastMarked :: Board -> [Int] -> Int
findLastMarked board numbers = last markedNumbers
  where
    markedNumbers = filter (\number -> containsMarked board number) numbers

containsMarked :: Board -> Int -> Bool
containsMarked board number = any (\field -> isMarkedValue field number) (concat board)
  where
    isMarkedValue (Marked value) number | value == number = True
                                        | otherwise = False
    isMarkedValue (Free _) number = False

data Field = Free Int | Marked Int
  deriving (Show, Eq)
type Board = [[Field]]

