module Day5 where

-- {{{ Imports

import Prelude

import Data.Array (foldM, (!!))
import Data.Array as Array
import Data.Array.NonEmpty (index)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Regex (Regex, match)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Day5Input as Day5Input
import Debug (spy)
import Effect (Effect)
import Effect.Console (logShow)

-- }}}

testRegex :: Regex
testRegex = unsafeRegex """(\[[A-Z]\]|   ) (\[[A-Z]\]|   ) (\[[A-Z]\]|   )""" RegexFlags.noFlags

realRegex :: Regex
realRegex = unsafeRegex """(\[[A-Z]\]|   ) (\[[A-Z]\]|   ) (\[[A-Z]\]|   ) (\[[A-Z]\]|   ) (\[[A-Z]\]|   ) (\[[A-Z]\]|   ) (\[[A-Z]\]|   ) (\[[A-Z]\]|   ) (\[[A-Z]\]|   )""" RegexFlags.noFlags

regexForSize :: Int -> Maybe Regex
regexForSize 3 = Just testRegex
regexForSize 9 = Just realRegex
regexForSize _ = Nothing

type ProblemInput =
  { stacks :: Array (Array String)
  , moves :: Array Move
  }

type Move =
  { count :: Int
  , from :: Int
  , to :: Int
  }

readStack :: Int -> String -> Maybe (Array String)
readStack num input = do
  regex <- regexForSize num
  m <- Regex.match regex input
  let { tail } = NonEmptyArray.uncons m
  if Array.length tail == num then sequence tail
  else Nothing

pushStack :: Array (Array String) -> Array String -> Maybe (Array (Array String))
pushStack stacks stack = traverseWithIndex (\i a -> case stack !! i of
                                             Just "   " -> Just a
                                             Nothing -> Nothing
                                             Just crate -> Just $ (Array.snoc a crate)) stacks

loadStacks :: Array (Array String) -> Array String -> Maybe (Array (Array String))
loadStacks stacks [] = Just stacks
loadStacks stacks lines = do
  { head, tail } <- Array.uncons lines
  stack <- readStack (Array.length stacks) head
  newStacks <- pushStack stacks stack
  loadStacks newStacks tail

parseStacks :: String -> Maybe (Array (Array String))
parseStacks input = do
  let lines = Array.reverse $ String.split (Pattern "\n") input
  { head, tail } <- Array.uncons lines
  numStacks <- String.split (Pattern "") head # \a -> Array.index a ((String.length head) - 2) >>= fromString
  loadStacks (Array.replicate numStacks []) tail

moveRegex :: Regex
moveRegex = unsafeRegex """move (\d+) from (\d+) to (\d+)""" RegexFlags.noFlags

parseMove :: String -> Maybe Move
parseMove input = do
  m <- Regex.match moveRegex input
  count <- join (m `index` 1) >>= fromString
  from <- join (m `index` 2) >>= fromString
  to <- join (m `index` 3) >>= fromString
  Just { count, from: from-1, to: to-1 }

parseMoves :: String -> Maybe (Array Move)
parseMoves = String.split (Pattern "\n") >>> traverse parseMove

parseInput :: String -> Maybe ProblemInput
parseInput input = do
  let halves = String.split (Pattern "\n\n") input
  stacks <- halves !! 0 >>= parseStacks
  moves <- halves !! 1 >>= parseMoves
  Just { stacks, moves }

transfer :: Int -> Array String -> Array String -> Maybe { newFrom:: Array String, newTo:: Array String }
transfer 0 from to = Just { newFrom: from, newTo: to }
transfer count from to = do
  { init, last } <- Array.unsnoc from
  transfer (count-1) init (Array.snoc to last)

transferInOrder :: Int -> Array String -> Array String -> Maybe { newFrom:: Array String, newTo:: Array String }
transferInOrder count from to =
  let
      { before, after } = Array.splitAt ((Array.length from) - count) from
      newFrom = before
      newTo = to <> after
   in if Array.length after == count then Just { newFrom, newTo } else Nothing

applyMove :: Boolean -> Array (Array String) -> Move -> Maybe (Array (Array String))
applyMove inOrder stacks {count, from, to} = do
  fromA <- stacks !! from
  toA <- stacks !! to
  { newFrom, newTo } <- if inOrder then transferInOrder count fromA toA else transfer count fromA toA
  Just $ Array.mapWithIndex (\i a -> if i == from then newFrom else if i == to then newTo else a) stacks

solve1 :: String -> Maybe String
solve1 input = do
  { stacks, moves } <- parseInput input
  finalStacks <- foldM (applyMove false) stacks moves
  traverse Array.last finalStacks <#> Array.intercalate "" <#> String.replaceAll (Pattern "[") (Replacement "") <#> String.replaceAll (Pattern "]") (Replacement "")

solve2 :: String -> Maybe String
solve2 input = do
  { stacks, moves } <- parseInput input
  finalStacks <- foldM (applyMove true) stacks moves
  traverse Array.last finalStacks <#> Array.intercalate "" <#> String.replaceAll (Pattern "[") (Replacement "") <#> String.replaceAll (Pattern "]") (Replacement "")

main :: Effect Unit
main = do
  logShow $ solve1 Day5Input.realInput
  logShow $ solve2 Day5Input.realInput
