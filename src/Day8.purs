module Day8 where

-- {{{ Imports

import Prelude

import Data.Array as Array
import Data.Foldable (foldM, maximum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Day8Input as Day8Input
import Effect (Effect)
import Effect.Console (logShow)
import Geometry (Dimensions(..), Direction(..), Grid, Point, getLine, gridDimensions, gridFromIntStrings, gridValueAt, makePoints)

-- }}}

parseInput :: String -> Maybe (Grid Int)
parseInput = gridFromIntStrings

isVisibleDirection :: Grid Int -> Point -> Direction -> Maybe Boolean
isVisibleDirection grid p direction = do
  values <- getLine grid p direction
  myValue <- gridValueAt p grid
  maxValue <- maximum values
  Just $ (maxValue < myValue)

scenicScoreDirection :: Grid Int -> Point -> Direction -> Maybe Int
scenicScoreDirection grid p direction = do
  values <- getLine grid p direction
  myValue <- gridValueAt p grid
  let firstMatch = Array.findIndex (\i -> i >= myValue) values
  case firstMatch of
      Just i -> Just (i + 1)
      _ -> Just (Array.length values)

scenicScore :: Grid Int -> Point -> Maybe Int
scenicScore grid p = do
  up <- scenicScoreDirection grid p Up
  down <- scenicScoreDirection grid p Down
  left <- scenicScoreDirection grid p Left
  right <- scenicScoreDirection grid p Right
  Just $ up * down * left * right

isVisible :: Grid Int -> Point -> Maybe Boolean
isVisible grid p =
  case isVisibleDirection grid p Up of
    Just true -> Just true
    _ -> case isVisibleDirection grid p Down of
      Just true -> Just true
      _ -> case isVisibleDirection grid p Left of
        Just true -> Just true
        _ -> case isVisibleDirection grid p Right of
          Just true -> Just true
          _ -> Just false

countVisible :: Grid Int -> Maybe Int
countVisible grid = do
  (Dimensions { height, width }) <- gridDimensions grid
  let
    edgeVisible = 2 * width + 2 * height - 4
    xs = Array.range 1 (width - 2)
    ys = Array.range 1 (height - 2)
    points = makePoints xs ys
  foldM addVisible edgeVisible points
  where
  addVisible sum p = do
    visible <- isVisible grid p
    if visible then Just (sum + 1) else Just sum

highestScenicScore :: Grid Int -> Maybe Int
highestScenicScore grid = do
  (Dimensions { height, width }) <- gridDimensions grid
  let
    xs = Array.range 1 (width - 2)
    ys = Array.range 1 (height - 2)
    points = makePoints xs ys
  traverse (scenicScore grid) points >>= maximum

main :: Effect Unit
main = do
  logShow $ parseInput Day8Input.realInput >>= countVisible
  logShow $ parseInput Day8Input.realInput >>= highestScenicScore
