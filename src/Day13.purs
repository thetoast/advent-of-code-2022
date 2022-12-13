module Day13 where

-- {{{ Imports

import Prelude

import Data.Argonaut.Core (Json, isArray, toArray, toNumber)
import Data.Argonaut.Parser (jsonParser)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..), fromRight, note)
import Data.Foldable (foldM)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Day13Input as Day13
import Effect (Effect)
import Effect.Console (logShow)

-- }}}

parseLine :: String -> Either String (Array Json)
parseLine = jsonParser >=> (toArray >>> note "Could not convert to array")

upgradeAndCompare :: Json -> Json -> Either String Ordering
upgradeAndCompare l r = do
  la <- if isArray l then toArray l # note "could not convert l to array" else pure [ l ]
  ra <- if isArray r then toArray r # note "could not vonert r to array" else pure [ r ]
  compareArrays la ra

compareAtIndex :: Array Json -> Array Json -> Int -> Either String Ordering
compareAtIndex l r i = do
  lv <- l !! i # note ("l has no index " <> show i)
  rv <- r !! i # note ("r has no index " <> show i)
  let
    lIsA = isArray lv
    rIsA = isArray rv
    mustUpgrade = lIsA && not rIsA || not lIsA && rIsA
  if mustUpgrade then upgradeAndCompare lv rv
  else if lIsA && rIsA then do
    la <- toArray lv # note "could not covert lhs to array"
    ra <- toArray rv # note "could not convert rhs to array"
    compareArrays la ra
  else do
    li <- toNumber lv # note "could not convert lhs to number"
    ri <- toNumber rv # note "could not convert rhs to number"
    pure $ compare li ri

compareArrays :: Array Json -> Array Json -> Either String Ordering
compareArrays [] [] = Right EQ
compareArrays [] _ = Right LT
compareArrays _ [] = Right GT
compareArrays l r = Array.range 0 ((Array.length l) - 1) # foldM check EQ # case _ of
  Right EQ -> if Array.length l < Array.length r then Right LT else Right EQ
  o -> o
  where
  check a i =
    if (a == GT) then Right GT
    else if (a == LT) then Right LT
    else if i >= Array.length l then Right LT
    else if i >= Array.length r then Right GT
    else compareAtIndex l r i

compareInputs :: String -> String -> Either String Ordering
compareInputs l r = do
  la <- parseLine l
  ra <- parseLine r
  compareArrays la ra

parseInput :: String -> Array (Array String)
parseInput = String.split (Pattern "\n\n") >>> map (String.split (Pattern "\n"))

solve1 :: String -> Either String Int
solve1 = parseInput >>> traverse checkPair >>> map (foldlWithIndex (\i sum a -> if a == LT then sum + i + 1 else sum) 0)
  where
  checkPair [ l, r ] = compareInputs l r
  checkPair _ = Left "cannot check a non-pair"

sortInputs :: String -> Array String
sortInputs = parseInput >>> Array.concat >>> Array.sortBy (\a b -> compareInputs a b # fromRight LT)

solve2 :: String -> Maybe Int
solve2 input = do
  let sorted = sortInputs ("[[2]]\n[[6]]\n\n" <> input)
  two <- Array.findIndex (eq "[[2]]") sorted <#> add 1
  six <- Array.findIndex (eq "[[6]]") sorted <#> add 1
  Just (two * six)

main :: Effect Unit
main = do
  logShow $ solve1 Day13.testInput
  logShow $ solve1 Day13.realInput
  logShow $ solve2 Day13.testInput
  logShow $ solve2 Day13.realInput
