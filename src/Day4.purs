module Day4 where

-- {{{ Imports

import Prelude

import Data.Array as Array
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Data.String (Pattern(..))
import Data.Tuple (Tuple(..))
import Day4Input as Day4Input
import Effect (Effect)
import Effect.Console (logShow)
import Parsing (customTupleParser, linesFrom)

-- }}}

type Assignment = (Tuple Int Int)
type ElfPair = (Tuple Assignment Assignment)

parseAssignment :: String -> Maybe Assignment
parseAssignment = customTupleParser (Pattern "-") fromString fromString

parsePair :: String -> Maybe ElfPair
parsePair = customTupleParser (Pattern ",") parseAssignment parseAssignment

parseInput :: String -> Maybe (Array ElfPair)
parseInput = linesFrom parsePair

fullyContains :: ElfPair -> Boolean
fullyContains (Tuple (Tuple al ar) (Tuple bl br))
  | al <= bl, br <= ar = true
  | bl <= al, ar <= br = true
  | otherwise = false

overlaps :: ElfPair -> Boolean
overlaps pair@(Tuple (Tuple al ar) (Tuple bl br))
  | al <= bl, ar >= bl = true
  | ar >= br, al <= br = true
  | otherwise = fullyContains pair

solve1 :: String -> Maybe Int
solve1 = parseInput >>> map (Array.filter fullyContains) >>> map Array.length

solve2 :: String -> Maybe Int
solve2 = parseInput >>> map (Array.filter overlaps) >>> map Array.length

main :: Effect Unit
main = do
  logShow $ solve2 Day4Input.realInput
