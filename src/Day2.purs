module Day2 where

import Prelude

import Data.Array ((!!))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Day2Input as Day2Input
import Effect (Effect)
import Effect.Console (logShow)

data Shape
  = Rock
  | Paper
  | Scissors

data Result
  = Lose
  | Draw
  | Win

derive instance Eq Shape

data Match = Match (Tuple Shape Shape)

scoreForShape :: Shape -> Int
scoreForShape Rock = 1
scoreForShape Paper = 2
scoreForShape Scissors = 3

scoreForMatch :: Match -> Int
scoreForMatch (Match (Tuple Rock r@Scissors)) = 0 + (scoreForShape r)
scoreForMatch (Match (Tuple Scissors r@Paper)) = 0 + (scoreForShape r)
scoreForMatch (Match (Tuple Paper r@Rock)) = 0 + (scoreForShape r)
scoreForMatch (Match (Tuple Rock r@Rock)) = 3 + (scoreForShape r)
scoreForMatch (Match (Tuple Paper r@Paper)) = 3 + (scoreForShape r)
scoreForMatch (Match (Tuple Scissors r@Scissors)) = 3 + (scoreForShape r)
scoreForMatch (Match (Tuple Paper r@Scissors)) = 6 + (scoreForShape r)
scoreForMatch (Match (Tuple Rock r@Paper)) = 6 + (scoreForShape r)
scoreForMatch (Match (Tuple Scissors r@Rock)) = 6 + (scoreForShape r)

shapeFor :: String -> Maybe Shape
shapeFor "A" = Just Rock
shapeFor "B" = Just Paper
shapeFor "C" = Just Scissors
shapeFor "X" = Just Rock
shapeFor "Y" = Just Paper
shapeFor "Z" = Just Scissors
shapeFor _ = Nothing

resultFor :: String -> Maybe Result
resultFor "X" = Just Lose
resultFor "Y" = Just Draw
resultFor "Z" = Just Win
resultFor _ = Nothing

shapeForResult :: Shape -> Result -> Shape
shapeForResult Rock Win = Paper
shapeForResult Rock Lose = Scissors
shapeForResult Paper Win = Scissors
shapeForResult Paper Lose = Rock
shapeForResult Scissors Win = Rock
shapeForResult Scissors Lose = Paper
shapeForResult l Draw = l

parseLine :: String -> Maybe Match
parseLine line = do
  let parts = split (Pattern " ") line
  left <- parts !! 0 >>= shapeFor
  right <- parts !! 1 >>= shapeFor
  Just $ Match (Tuple left right)

parseLine2 :: String -> Maybe Match
parseLine2 line = do
  let parts = split (Pattern " ") line
  left <- parts !! 0 >>= shapeFor
  right <- parts !! 1 >>= resultFor <#> shapeForResult left
  Just $ Match (Tuple left right)

parseInput :: String -> Maybe (Array Match)
parseInput = split (Pattern "\n") >>> traverse parseLine

parseInput2 :: String -> Maybe (Array Match)
parseInput2 = split (Pattern "\n") >>> traverse parseLine2

part1 :: String -> Maybe Int
part1 = parseInput <#> map (foldl (\t m -> t + scoreForMatch m) 0)

part2 :: String -> Maybe Int
part2 = parseInput2 <#> map (foldl (\t m -> t + scoreForMatch m) 0)

main :: Effect Unit
main = do
  logShow $ part2 Day2Input.realInput
