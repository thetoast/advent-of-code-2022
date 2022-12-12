module Day12 where

-- {{{ Imports

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), isJust)
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
import Geometry (Grid, NeighborType(..), Point(..), gridFromPoints, gridValueAt, insertGridAt, toGrid, validNeighbors)

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
  start <- foldlWithIndex (findCode startCode) Nothing g
  end <- foldlWithIndex (findCode endCode) Nothing g
  grid <- toGrid g >>= insertGridAt start (aCode - 1) >>= insertGridAt end (zCode + 1)
  pure { grid: grid, start, end }
  where
  findCode code y acc v =
    if isJust acc then acc
    else case Array.findIndex (eq code) v of
      Just x -> Just (Point { x, y })
      Nothing -> Nothing

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

main :: Effect Unit
main = do
  logShow $ parseInput Day12Input.realInput >>= findShortestPath <#> spyWith "grid" (\p -> gridFromPoints p (Tuple "." "#") # show) >>> Array.length
