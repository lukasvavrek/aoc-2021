#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

-- test_input.txt <- 900
-- input.txt <- 1739283308

main :: IO ()
main = do
  content <- readFile "input.txt"
  let directions = map (\command -> parseCommand command) (lines content)
  let state = foldl (\state' direction -> executeDirection state' direction) defaultState directions
  let result = (position state) * (depth state)
  putStrLn (show result)
  where
    defaultState = State{aim=0, depth=0, position=0}

parseCommand :: String -> (String, Int)
parseCommand command' = (command, value)
  where
    split = words command'
    command = split !! 0
    value = read (split !! 1) :: Int

executeDirection :: State -> (String, Int) -> State
executeDirection state ("down", value) = state {aim=(aim state) + value}
executeDirection state ("up", value) = state {aim=(aim state) - value}
executeDirection state ("forward", value) =
  state {
    position=(position state) + value,
    depth=(depth state) + ((aim state) * value)
  }

data State = State {
  aim :: Int,
  depth :: Int,
  position :: Int
} deriving (Show, Eq)

