module Day12 where

-- {{{ Imports

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (toChar)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Day12Input as Day12Input
import Debug (spyWith)
import Effect (Effect)
import Effect.Console (logShow)
import Geometry (Grid, NeighborType(..), Point, findAllInGrid, findInGrid, gridFromPoints, gridValueAt, insertGridAt, toGrid, validNeighbors)

-- }}}

type InputMap =
  { grid :: Grid Int
  , start :: Point
  , end :: Point
  }

toInputMap :: Array (Array Int) -> Maybe InputMap
toInputMap g = do
  let
    startCode = toCharCode 'S'
    endCode = toCharCode 'E'
    aCode = toCharCode 'a'
    zCode = toCharCode 'z'
  grid <- toGrid g
  start <- findInGrid grid startCode
  end <- findInGrid grid endCode
  newGrid <- insertGridAt start (aCode - 1) grid >>= insertGridAt end (zCode + 1)
  pure { grid: newGrid, start, end }

parseInput :: String -> Maybe InputMap
parseInput = String.split (Pattern "\n") >>> map (String.split (Pattern "")) >>> traverse (traverse (toChar >>> map toCharCode)) >=> toInputMap

type State =
  { queue :: List (Tuple Point (Array Point))
  , visited :: Set Point
  }

initState :: InputMap -> State
initState { start } = { queue: Cons (Tuple start []) Nil, visited: Set.empty }

findShortestPath :: InputMap -> Maybe (Array Point)
findShortestPath s@{ grid, end } = tailRecM go (initState s)
  where
  go { queue, visited } = do
    { head: Tuple p path, tail } <- List.uncons queue
    if p == end then Just (Done path)
    else if Set.member p visited then Just (Loop { queue: tail, visited })
    else do
      myValue <- gridValueAt p grid
      let newPath = Array.snoc path p
      neighbors <- validNeighbors p Adjacent grid # Array.filterA (flip gridValueAt grid >>> map (_ <= (myValue + 1)))
      let
        newQueue = tail <> ((neighbors <#> flip Tuple newPath) # List.fromFoldable)
        newVisited = Set.insert p visited
      Just (Loop { queue: newQueue, visited: newVisited })

solve2 :: InputMap -> Maybe (Array Point)
solve2 s@{ grid } = findAllInGrid grid (toCharCode 'a') >>= foldl checkShortest Nothing
  where
  checkShortest a p = case findShortestPath (s { start = p }) of
    Just path -> case a of
      Just shortest -> if Array.length path < Array.length shortest then Just path else a
      Nothing -> Just path
    _ -> a

main :: Effect Unit
main = do
  --logShow $ parseInput Day12Input.realInput >>= findShortestPath <#> spyWith "grid" (\p -> gridFromPoints p (Tuple "." "#") # show) >>> Array.length
  logShow $ parseInput Day12Input.realInput >>= solve2 <#> spyWith "grid" (\p -> gridFromPoints p (Tuple "." "#") # show) >>> Array.length
