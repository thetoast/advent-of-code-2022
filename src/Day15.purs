module Day15 where

-- {{{ Imports

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Array.NonEmpty (index)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Set as Set
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Day15Input as Day15Input
import Effect (Effect)
import Effect.Console (logShow)
import Geometry (Point(..))
import Parsing (linesFrom)

--- }}}

lineRegex :: Regex
lineRegex = unsafeRegex """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""" noFlags

type Sensor = Point
type Beacon = Point
type Reading = (Tuple Sensor Beacon)

type YCoverage = Array (Tuple Int Int)

parseLine :: String -> Maybe Reading
parseLine = match lineRegex >=> \m -> do
  xSensor <- m `index` 1 # join >>= Int.fromString
  ySensor <- m `index` 2 # join >>= Int.fromString
  xBeacon <- m `index` 3 # join >>= Int.fromString
  yBeacon <- m `index` 4 # join >>= Int.fromString
  pure $ (Point { x: xSensor, y: ySensor }) /\ (Point { x: xBeacon, y: yBeacon })

manhattan :: Point -> Point -> Int
manhattan (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) = (abs (x2 - x1)) + (abs (y2 - y1))

sensorCoverageAt :: Reading -> Int -> YCoverage
sensorCoverageAt (p1@(Point { x: xSensor, y: ySensor }) /\ p2) yCov =
  let
    dist = manhattan p1 p2
    mkRange n distance = Tuple (n - distance) (n + distance)
  in
    [ mkRange xSensor (dist - (abs (ySensor - yCov))) ]

sensorCoverageAtRange :: Reading -> Int -> Int -> YCoverage
sensorCoverageAtRange (p1@(Point { x: xSensor, y: ySensor }) /\ p2) yCov maxX =
  let
    dist = manhattan p1 p2
    mkRange n distance = Tuple (max (n - distance) 0) (min (n + distance) maxX)
  in
    [ mkRange xSensor (dist - (abs (ySensor - yCov))) ]

merge :: YCoverage -> YCoverage
merge = Array.sortWith fst >>> go
  where
  go arr =
    case Array.uncons arr of
      Nothing -> arr
      Just { head: head@(Tuple hl hr), tail } -> case Array.uncons tail of
        Nothing -> arr
        Just { head: (Tuple nl nr), tail: rest } ->
          if hr >= nl then go $ Array.cons (Tuple (min hl nl) (max hr nr)) rest
          else Array.cons head (go tail)

coversY :: Int -> Reading -> Boolean
coversY yCov (sensor@(Point { y }) /\ beacon) =
  let
    dist = manhattan sensor beacon
    minY = y - dist
    maxY = y + dist
  in
    minY <= yCov && yCov <= maxY

combineCoverage :: YCoverage -> YCoverage -> YCoverage
combineCoverage a b = merge (a <> b)

parseInput :: String -> Maybe (Array Reading)
parseInput = linesFrom parseLine

produceCoverage :: Array Reading -> Int -> YCoverage
produceCoverage readings yCov = Array.filter (coversY yCov) readings # foldl (\a r -> sensorCoverageAt r yCov # combineCoverage a) []

produceCoverageBounds :: Array Reading -> Int -> Int -> YCoverage
produceCoverageBounds readings yCov maxX = Array.filter (coversY yCov) readings # foldl (\a r -> sensorCoverageAtRange r yCov maxX # combineCoverage a) []

solve1 :: String -> Int -> Maybe Int
solve1 input y = do
  readings <- parseInput input
  let
    coverage = produceCoverage readings y
    pointsOnY = foldl checkY Set.empty readings # Set.size
    checkY a (s@(Point sp) /\ b@(Point bp)) =
      let
        withSensor = if sp.y == y then Set.insert s a else a
        withBeacon = if bp.y == y then Set.insert b withSensor else withSensor
      in
        withBeacon
  pure $ foldl (\a (Tuple l r) -> a + (abs (r - l)) + 1) 0 coverage # flip (-) pointsOnY

findSingleGap :: YCoverage -> Int -> Maybe Int
findSingleGap a maxX = case Array.take 2 a of
  [ l, r ] | (fst l) == 0, (snd r) == maxX, (fst r) - (snd l) == 2 -> Just ((fst r) - 1)
  _ -> Nothing

scanCoverageY :: Array Reading -> Int -> Int -> Maybe Int
scanCoverageY readings maxX y = do
  let coverage = produceCoverageBounds readings y maxX
  findSingleGap coverage maxX

scanCoverage :: Array Reading -> Int -> Int -> Maybe BigInt
scanCoverage readings maxX maxY = tailRecM go 0
  where
  go y
    | y == maxY = case scanCoverageY readings maxX y of
        Just result -> Just (Done $ ((BigInt.fromInt result) * (BigInt.fromInt 4000000) + (BigInt.fromInt y)))
        Nothing -> Nothing
    | otherwise = case scanCoverageY readings maxX y of
        Just result -> Just (Done $ ((BigInt.fromInt result) * (BigInt.fromInt 4000000) + (BigInt.fromInt y)))
        Nothing -> Just (Loop (y + 1))

solve2 :: String -> Int -> Int -> Maybe BigInt
solve2 input maxX maxY = do
  readings <- parseInput input
  scanCoverage readings maxX maxY

main :: Effect Unit
main = do
  logShow $ solve2 Day15Input.testInput 20 20
  logShow $ solve2 Day15Input.realInput 4000000 4000000
