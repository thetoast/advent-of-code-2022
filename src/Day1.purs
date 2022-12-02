module Day1 where

import Prelude

import Data.Array (snoc, sortBy, sortWith, take)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Day1Input as Day1Input
import Effect (Effect)
import Effect.Console (logShow)

type Inventory =
  { items :: Array Int
  , total :: Int
  }

type ParseState =
  { curr :: Inventory
  , all :: Array Inventory
  }

empty :: Inventory
empty = { items: [], total: 0 }

processEntry :: ParseState -> String -> ParseState
processEntry a@{ curr, all } "" = a { curr = empty, all = snoc all curr }
processEntry a@{ curr: { items, total } } s = case fromString s of
  Just i -> a { curr = { items: snoc items i, total: total + i } }
  Nothing -> a

parseInput :: String -> Array Inventory
parseInput = split (Pattern "\n") >>> foldl processEntry { curr: empty, all: [] } >>> \{curr, all} -> snoc all curr

part1 :: Effect Unit
part1 = do
  let elves = parseInput Day1Input.realInput
  logShow $ foldl (\highest { total } -> max highest total) 0 elves

part2 :: Effect Unit
part2 = do
  let elves = parseInput Day1Input.realInput # sortWith _.total # take 3
  logShow $ foldl (\a { total } -> a + total) 0 elves

main :: Effect Unit
main = do
  part2
