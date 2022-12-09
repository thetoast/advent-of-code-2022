module Day9 where

-- {{{ Imports

import Prelude

import Data.Array as Array
import Data.Foldable (foldM, foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Day9Input as Day9Input
import Debug (spy)
import Effect (Effect)
import Effect.Console (logShow)
import Geometry (Point(..), gridFromPoints)
import Parsing (linesFrom, tupleParser)

-- }}}

type Move = Tuple Direction Steps
type Steps = Int
type Direction = Point

type State =
  { knots :: Array Point
  , visited :: Set Point
  }

origin :: Point
origin = Point ({ x: 0, y: 0 })

initState :: State
initState =
  { knots: Array.replicate 10 origin
  , visited: Set.singleton origin
  }

parseDir :: String -> Maybe Direction
parseDir "U" = Just (Point { x: 0, y: -1 })
parseDir "D" = Just (Point { x: 0, y: 1 })
parseDir "L" = Just (Point { x: -1, y: 0 })
parseDir "R" = Just (Point { x: 1, y: 0 })
parseDir _ = Nothing

parseMove :: String -> Maybe Move
parseMove = tupleParser parseDir fromString

parseInput :: String -> Maybe (Array Move)
parseInput = linesFrom parseMove

calculateDirection :: Point -> Point -> Direction
calculateDirection (Point { x: hx, y: hy }) (Point { x: tx, y: ty }) =
  let
    dx = (hx - tx)
    dy = (hy - ty)
    needsMove = (abs dx) > 1 || (abs dy) > 1
  in
    if needsMove then
      let
        moveX = clamp (-1) 1 (hx - tx)
        moveY = clamp (-1) 1 (hy - ty)
      in
        Point { x: moveX, y: moveY }
    else zero

moveTo :: Point -> Point -> Point
moveTo target point =
  let
    pMove = calculateDirection target point
    in point + pMove

moveKnots :: Array Point -> Maybe (Array Point)
moveKnots a@[ _ ] = Just a
moveKnots knots = do
  { head, tail } <- Array.uncons knots
  { head: next, tail: rest } <- Array.uncons tail
  let newNext = moveTo head next
  newTail <- moveKnots (Array.cons newNext rest)
  Just (Array.cons head newTail)

move :: State -> Move -> Maybe State
move state (Tuple direction steps) = do
  { head, tail } <- Array.uncons state.knots
  let newHead = head + direction
  newKnots <- moveKnots (Array.cons newHead tail)
  last <- Array.last newKnots
  let newState = { knots: newKnots, visited: Set.insert last state.visited }
  case steps of
    1 -> Just newState
    _ -> move newState (Tuple direction (steps - 1))

solve1 :: String -> Maybe Int
solve1 = parseInput >=> foldM move initState >>> map _.visited >>> map Set.size

main :: Effect Unit
main = do
  logShow $ solve1 Day9Input.realInput
