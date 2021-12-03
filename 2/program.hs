#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

-- test_input.txt <- 150
-- input.txt <- 1766

-- direction is not helpfull, but I wanted to practice

main :: IO ()
main = do
  content <- readFile "input.txt"
  let directions = map (\command -> parseCommand command) (lines content)
  let (x, y) = executeDirections directions
  let result = x*y
  putStrLn (show result)

parseCommand :: String -> Direction
parseCommand command =
  let
    split = words command
    direction' = split !! 0
    value' = read (split !! 1) :: Integer
  in
    case direction' of
      "forward" -> Forward value'
      "up" -> Up value'
      "down" -> Down value'

executeDirections :: [Direction] -> (Integer, Integer)
executeDirections [] = (0, 0)
executeDirections (x:xs) =
  let
    (l, d) = case x of
      Forward n     -> (n, 0)
      Up n          -> (0, -n)
      Down n        -> (0, n)
    (l', d') = executeDirections xs
  in
    (l+l', d+d')

data Direction = Forward Integer | Down Integer | Up Integer
  deriving (Show, Eq)

