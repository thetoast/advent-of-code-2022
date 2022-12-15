module Day14 where

-- {{{ Imports

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Day14Input as Day14
import Effect (Effect)
import Effect.Console (logShow)
import Geometry (Grid, Point(..), gridFromPoints, gridValueAt, insertGridAt, makePoints, pointFromString)

-- }}}

pointLine :: Point -> Point -> Either String (Array Point)
pointLine (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) =
  if x1 == x2 || y1 == y2 then do
    let
      xs = Array.range x1 x2
      ys = Array.range y1 y2
    pure $ makePoints xs ys
  else Left "diagonal lines are invalid"

type Markers =
  { empty :: String
  , sand :: String
  , source :: String
  , rock :: String
  }

markers :: Markers
markers = { empty: ".", sand: "o", source: "+", rock: "#" }

type Chart =
  { grid :: Grid String
  , source :: Point
  , settled :: Int
  , reachedAbyss :: Boolean
  }

makeLines :: Array Point -> Either String (Array Point)
makeLines [] = pure []
makeLines [ _ ] = pure []
makeLines points = do
  { head: p1, tail: tail } <- Array.uncons points # note "no points left"
  { head: p2 } <- Array.uncons tail # note "not enough points left"
  pointLine p1 p2 <> makeLines tail

findOrigin :: Array Point -> Either String Point
findOrigin points = do
  (Point { x }) <- points !! 0 # note "cannot find origin for empty points"
  let { minX, minY } = foldl updateMin { minX: x, minY: 0 } points
  pure $ Point { x: minX, y: minY }
  where
  updateMin a (Point { x, y }) =
    { minX: min x a.minX
    , minY: min y a.minY
    }

normalizePoints :: Array Point -> Point -> Array Point
normalizePoints points origin = map (\p -> p - origin) points

makeChart :: Array (Array Point) -> Either String Chart
makeChart points = do
  lines <- traverse makeLines points
  let allPoints = Array.concat lines
  origin <- findOrigin allPoints
  let normalized = normalizePoints allPoints origin
  chart <- gridFromPoints normalized (markers.empty /\ markers.rock) # note "unable to construct grid"
  let source = (Point { x: 500, y: 0 }) - origin
  grid <- insertGridAt source markers.source chart # note "unable to insert source into grid"
  pure { grid, source, settled: 0, reachedAbyss: false }

parseLine :: String -> Either String (Array Point)
parseLine = String.split (Pattern " -> ") >>> traverse (pointFromString >>> note "unable to parse point")

parseInput :: String -> Either String Chart
parseInput = String.split (Pattern "\n") >>> traverse parseLine >=> makeChart

parseInput2 :: String -> Either String Chart
parseInput2 = String.split (Pattern "\n") >>> traverse parseLine >=> makeChart2

findMaxY :: Array Point -> Either String Int
findMaxY points = do
  (Point { y: yStart }) <- points !! 0 # note "can't find maxY of empty list"
  pure $ foldl (\a (Point { y }) -> max a y) yStart points

makeChart2 :: Array (Array Point) -> Either String Chart
makeChart2 points = do
  lines <- traverse makeLines points
  let allPoints = Array.concat lines
  maxY <- findMaxY allPoints <#> add 2
  let
    minX = 500 - maxY - 2
    maxX = 500 + maxY + 2
  floor <- pointLine (Point { x: minX, y: maxY }) (Point { x: maxX, y: maxY })
  let pointsWithFloor = allPoints <> floor
  origin <- findOrigin pointsWithFloor
  let normalized = normalizePoints pointsWithFloor origin
  chart <- gridFromPoints normalized (markers.empty /\ markers.rock) # note "unable to construct grid"
  let source = (Point { x: 500, y: 0 }) - origin
  grid <- insertGridAt source markers.source chart # note "unable to insert source into grid"
  pure { grid, source, settled: 0, reachedAbyss: false }

settleSand :: Chart -> Either String Chart
settleSand chart@{ grid, source } = go source
  where
  go p@(Point { x, y }) = do
    let
      down = (Point { x: x, y: y + 1 })
      left = (Point { x: x - 1, y: y + 1 })
      right = (Point { x: x + 1, y: y + 1 })
    case gridValueAt source grid of
      Just "o" -> pure $ chart { reachedAbyss = true }
      _ -> case gridValueAt down grid of
        Just "." -> go down
        Nothing -> pure $ chart { reachedAbyss = true }
        _ -> case gridValueAt left grid of
          Just "." -> go left
          Nothing -> pure $ chart { reachedAbyss = true }
          _ -> case gridValueAt right grid of
            Just "." -> go right
            Nothing -> pure $ chart { reachedAbyss = true }
            _ -> insertGridAt p markers.sand chart.grid # note "failed to insert sand" <#> \g -> chart { grid = g, settled = chart.settled + 1 } -- settled

solve1 :: String -> Either String Chart
solve1 = parseInput >=> tailRecM go
  where
  go chart@{ reachedAbyss: true } = Right (Done chart)
  go chart = settleSand chart <#> Loop

solve2 :: String -> Either String Chart
solve2 = parseInput2 >=> tailRecM go
  where
  go chart@{ reachedAbyss: true } = Right (Done chart)
  go chart = settleSand chart <#> Loop

main :: Effect Unit
main = do
  --logShow $ solve2 Day14.testInput
  logShow $ solve2 Day14.realInput
