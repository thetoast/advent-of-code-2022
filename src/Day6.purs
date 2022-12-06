module Day6 where

-- {{{ Imports

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Day6Input as Day6Input
import Effect (Effect)
import Effect.Console (logShow)

-- }}}

isUnique :: Int -> String -> Boolean
isUnique count = String.split (Pattern "") >>> Set.fromFoldable >>> \s -> Set.size s == count

solve :: Int -> String -> Maybe Int
solve count input = go count input
  where
    go :: Int -> String -> Maybe Int
    go _ "" = Nothing
    go processed stream =
      let
          isStart = String.take count stream # isUnique count
          rest = String.drop 1 stream
       in if isStart then Just processed else go (processed + 1) rest

main :: Effect Unit
main = do
  logShow $ solve 4 Day6Input.realInput
  logShow $ solve 14 Day6Input.realInput
