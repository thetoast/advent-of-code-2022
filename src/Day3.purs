module Day3 where

-- {{{ Imports
import Prelude

import Data.Array (foldM, (!!))
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Foldable as Array
import Data.Map (Map, insertWith)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set, size)
import Data.Set as Set
import Data.String (split, Pattern(..))
import Data.String as String
import Data.String.CodeUnits (charAt)
import Data.Traversable (traverse)
import Day3Input as Day3Input
import Effect (Effect)
import Effect.Console (logShow)
import Parsing (linesFrom)

-- }}}

newtype Compartment = Compartment (Map String Int)

newtype Rucksack = Rucksack
  { c1 :: Compartment
  , c2 :: Compartment
  , all :: Compartment
  }

instance Show Compartment where
  show (Compartment m) = show m

instance Show Rucksack where
  show (Rucksack { c1, c2 }) = "{ " <> show c1 <> "\n, " <> show c2 <> "\n}"

loadCompartment :: String -> Compartment
loadCompartment = split (Pattern "") >>> foldl (\a k -> insertWith (+) k 1 a) Map.empty >>> Compartment

loadRucksack :: String -> Maybe Rucksack
loadRucksack input = do
  let len = String.length input
  case len `mod` 2 of
    1 -> Nothing
    _ -> do
      let { before, after } = String.splitAt (len / 2) input
      Just (Rucksack { c1: loadCompartment before, c2: loadCompartment after, all: loadCompartment input })

parseInput :: String -> Maybe (Array Rucksack)
parseInput = linesFrom loadRucksack

findIntersection :: Rucksack -> Maybe String
findIntersection (Rucksack { c1: (Compartment m1), c2: (Compartment m2) }) =
  let
    k1 = Map.keys m1
    k2 = Map.keys m2
    i = Set.intersection k1 k2
  in
    case size i of
      1 -> Set.toUnfoldable i !! 0
      _ -> Nothing

findIntersections :: Array Rucksack -> Maybe (Array String)
findIntersections = traverse findIntersection

lowerA :: Int
lowerA = 97

lowerZ :: Int
lowerZ = 122

upperA :: Int
upperA = 65

upperZ :: Int
upperZ = 90

toPriority :: Char -> Maybe Int
toPriority c = do
  let code = toCharCode c
  if lowerA <= code && code <= lowerZ then Just $ code - lowerA + 1
  else if upperA <= code && code <= upperZ then Just $ code - upperA + 27
  else Nothing

priorityOf :: String -> Maybe Int
priorityOf s
  | String.length s == 1 = charAt 0 s >>= toPriority
  | otherwise = Nothing

sumPriorities :: Array String -> Maybe Int
sumPriorities = traverse priorityOf >>> map Array.sum

solve1 :: String -> Maybe Int
solve1 = parseInput >=> findIntersections >=> sumPriorities

type SackState =
  { pass :: Int
  , intersections :: Set String
  , badges :: Array String
  }

findBadges :: Array Rucksack -> Maybe (Array String)
findBadges = foldM findIt { pass: 0, intersections: Set.empty, badges: [] } >>> map _.badges
  where
  findIt :: SackState -> Rucksack -> Maybe SackState
  findIt { pass, intersections, badges } (Rucksack { all: (Compartment m) }) =
    let
      curPass = pass + 1
    in
      case curPass of
        1 -> Just { pass: curPass, intersections: Map.keys m, badges }
        2 -> Just { pass: curPass, intersections: Set.intersection (Map.keys m) intersections, badges }
        3 ->
          let
            newIntersections = Set.intersection (Map.keys m) intersections
          in
            case Set.size newIntersections of
              1 -> Set.toUnfoldable newIntersections !! 0 <#> \b -> { pass: 0, intersections: Set.empty, badges: badges <> [ b ] }
              _ -> Nothing
        _ -> Nothing

solve2 :: String -> Maybe Int
solve2 = parseInput >=> findBadges >=> sumPriorities

main :: Effect Unit
main = do
  logShow $ solve1 Day3Input.realInput
  logShow $ solve2 Day3Input.realInput
