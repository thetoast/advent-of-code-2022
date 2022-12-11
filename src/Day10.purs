module Day10 where

-- {{{ Imports

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (foldM, foldl, sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Day10Input as Day10Input
import Debug (spy)
import Effect (Effect)
import Effect.Console (log, logShow)
import Parsing (linesFrom)

-- }}}

data Op
  = Noop
  | Addx Int

parseCommand :: String -> Maybe Op
parseCommand input = case String.split (Pattern " ") input of
  [ "noop" ] -> Just Noop
  [ "addx", i ] -> fromString i <#> Addx
  _ -> Nothing

type State =
  { cycles :: Int
  , x :: Int
  , signals :: Array Int
  , pixels :: Array Boolean
  }

initState :: State
initState =
  { cycles: 0
  , x: 1
  , signals: []
  , pixels: Array.replicate 240 false
  }

shouldLog :: Int -> Boolean
shouldLog 20 = true
shouldLog n
  | (n - 20) `mod` 40 == 0 = true
  | otherwise = false

signalValue :: Int -> Int -> Int
signalValue = (*)

toCrtPos :: Int -> Int
toCrtPos cycles = cycles `mod` 40

logPixel :: State -> Maybe State
logPixel state@{ cycles, pixels, x } = do
  let
    crtPos = toCrtPos (cycles - 1)
    pixelPos = (cycles - 1)
  newPixels <- if (x - 1) <= crtPos && crtPos <= (x + 1) then Array.updateAt pixelPos true pixels else Just pixels
  Just state { pixels = newPixels }

processOp :: State -> Op -> Maybe State
processOp state Noop = do
  let newCycles = state.cycles + 1
  updatedState <- logPixel $ state { cycles = newCycles }
  if shouldLog newCycles then Just updatedState { signals = Array.snoc state.signals (signalValue newCycles state.x) }
  else Just updatedState
processOp state (Addx i) = do
  let
    newCycles = state.cycles + 2
    newX = state.x + i
  updatedState <- (logPixel <<< \s -> s { cycles = newCycles }) =<< (logPixel $ state { cycles = state.cycles + 1 })
  if shouldLog (newCycles - 1) then Just updatedState { signals = Array.snoc state.signals (signalValue (newCycles - 1) state.x), x = newX }
  else if shouldLog (newCycles) then Just updatedState { signals = Array.snoc state.signals (signalValue newCycles state.x), x = newX }
  else Just updatedState { x = newX }

solve1 :: String -> Maybe Int
solve1 input = do
  ops <- linesFrom parseCommand input
  finalState <- foldM processOp initState ops
  Just $ sum finalState.signals

renderCRT :: State -> String
renderCRT { pixels } = mapWithIndex (\i b -> if toCrtPos i == 39 then (toPixel b) <> "\n" else (toPixel b)) pixels # Array.intercalate ""
  where
  toPixel b = if b then "#" else "."

solve2 :: String -> Maybe String
solve2 input = do
  ops <- linesFrom parseCommand input
  finalState <- foldM processOp initState ops
  Just $ renderCRT finalState

main :: Effect Unit
main = do
  logShow $ solve1 Day10Input.realInput
  case solve2 Day10Input.realInput of
    Just s -> log s
    Nothing -> log "oops"
