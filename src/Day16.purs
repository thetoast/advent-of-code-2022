module Day16 where

-- {{{ Imports

import Prelude

import Control.Monad.Rec.Class (Step(..), loop2, tailRec2)
import Data.Array as Array
import Data.Array.NonEmpty ((!!))
import Data.Foldable (foldM, foldl, sum)
import Data.Int as Int
import Data.List (toUnfoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Day16Input as Day16Input
import Debug (spy, spyWith, trace)
import Effect (Effect)
import Effect.Console (logShow)
import Graph (Graph)
import Graph as Graph

-- }}}

-- {{{ Sample Plan
samplePlan :: Plan
samplePlan =
  [ Move "DD"
  , Activate "DD" 20
  , Move "CC"
  , Move "BB"
  , Activate "BB" 13
  , Move "AA"
  , Move "II"
  , Move "JJ"
  , Activate "JJ" 21
  , Move "II"
  , Move "AA"
  , Move "DD"
  , Move "EE"
  , Move "FF"
  , Move "GG"
  , Move "HH"
  , Activate "HH" 22
  , Move "GG"
  , Move "FF"
  , Move "EE"
  , Activate "EE" 3
  , Move "DD"
  , Move "CC"
  , Activate "CC" 2
  ]

-- }}}

lineRegex :: Regex
lineRegex = unsafeRegex """Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)""" noFlags

type Cave = Graph Valve Rate

type Valve = String
type Rate = Int
data Action
  = Move Valve
  | Activate Valve Rate

instance Show Action where
  show (Move v) = "Move to " <> v
  show (Activate v r) = "Open value " <> v <> " at rate " <> show r

derive instance Eq Action

type Plan = Array Action

parseLine :: Cave -> String -> Maybe Cave
parseLine cave line = do
  m <- match lineRegex line
  name <- m !! 1 # join
  rate <- m !! 2 # join >>= Int.fromString
  edges <- m !! 3 # join <#> String.split (Pattern ", ")
  let withValue = Graph.setValue cave name rate
  pure $ foldl (insertEdge name) withValue edges
  where
  insertEdge from g to = Graph.addEdge g (Tuple from to)

parseInput :: String -> Maybe Cave
parseInput = String.split (Pattern "\n") >>> foldM parseLine Graph.emptyGraph

planResult :: Plan -> Int -> { rate :: Int, total :: Int, seconds :: Int }
planResult plan maxSeconds = foldl step initValue plan # \{ seconds: elapsed, rate, total } -> { seconds: maxSeconds, rate, total: total + (rate * (maxSeconds - elapsed)) }
  where
  initValue = { seconds: 0, rate: 0, total: 0 }
  step { seconds, rate, total } action =
    let
      newTotal = total + rate
      newSeconds = seconds + 1
    in
      case action of
        Move _ -> { seconds: newSeconds, rate, total: newTotal }
        Activate _ r -> { seconds: newSeconds, rate: rate + r, total: newTotal }

planValue :: Plan -> Int -> Int
planValue plan maxSeconds = planResult plan maxSeconds # _.total

sortPlans :: Array Plan -> Array Plan
sortPlans plans =
  let
    --maxLen = foldl max 0 $ map Array.length plans
    maxLen = 30
    comparePlans a b =
      let
        planResA = planResult a maxLen
        planResB = planResult b maxLen
      in
        case compare planResB.total planResA.total of
          EQ -> compare planResB.rate planResA.rate
          c -> c
  in
    Array.sortBy comparePlans plans

shortestPaths :: Cave -> Valve -> Map Valve Plan
shortestPaths cave start = tailRec2 go Map.empty [ (Tuple start []) ]
  where
  go paths queue =
    case Array.uncons queue of
      Nothing -> Done paths
      Just { head: (node /\ path), tail } ->
        let
          newPaths = if node == start then paths else Map.insert node path paths
          neighbors = Graph.nodeEdges node cave # Array.filter (\n -> not $ Map.member n paths)
          newQueue = tail <> (map (\n -> Tuple n (Array.snoc path (Move n))) neighbors)
        in
          loop2 newPaths newQueue

bestMoves :: Cave -> Valve -> Array Plan
bestMoves cave valve =
  let
    paths = shortestPaths cave valve # Map.values # toUnfoldable
    activations = paths <#> \a -> case Array.unsnoc a of
      Nothing -> [ Activate valve (Graph.getValue cave valve # fromMaybe 0) ]
      Just { last: (Move v) } -> Array.snoc a (Activate v (Graph.getValue cave v # fromMaybe 0))
      _ -> a
  in
    sortPlans activations # Array.filter
      ( \p -> case Array.unsnoc p of
          Just { last: (Activate _ 0) } -> false
          _ -> true
      )

bestMovesForAll :: Cave -> Map Valve (Array Plan)
bestMovesForAll cave = Graph.allNodes cave # foldl (\m node -> Map.insert node (bestMoves cave node) m) Map.empty

destination :: Plan -> Maybe Valve
destination plan = case Array.unsnoc plan of
  Just { last: Activate valve _ } -> pure valve
  Just { last: Move valve } -> pure valve
  _ -> Nothing

removeDestination :: Map Valve (Array Plan) -> Valve -> Map Valve (Array Plan)
removeDestination moves valve = Map.mapMaybe removeIt moves
  where
  removeIt plan = pure $ Array.filter (destination >>> map (eq valve >>> not) >>> fromMaybe false) plan

showPlan :: Plan -> String
showPlan = Array.filter isActivate >>> map toName >>> Array.intercalate ":"
  where
  isActivate (Activate _ _) = true
  isActivate _ = false
  toName (Activate v _) = v
  toName (Move v) = v

bestPlan :: Cave -> Int -> Maybe Plan
bestPlan cave seconds = bestMovesForAll cave # bestPlanWithMoves seconds

bestPlanWithMoves :: Int -> Map Valve (Array Plan) -> Maybe Plan
bestPlanWithMoves seconds moves' = go "AA" [] moves'
  where
  go node plan moves = do
    myMoves <- Map.lookup node moves
    let
      updatedMoves = removeDestination moves node
      comparePlan best move = do
        let newPlan = plan <> move
        if Array.length newPlan > seconds then pure best
        else do
          dest <- destination move
          destBest <- go dest newPlan updatedMoves
          pure $ if (planValue best seconds) < (planValue destBest seconds) then destBest else best
    foldM comparePlan plan myMoves

removeUsed :: Plan -> Map Valve (Array Plan) -> Map Valve (Array Plan)
removeUsed plan moves = Array.mapMaybe justActivations plan # foldl removeDestination moves
  where
  justActivations = case _ of
    (Activate v _) -> Just v
    _ -> Nothing

planPairValue :: Tuple Plan Plan -> Int -> Int
planPairValue (p1 /\ p2) seconds = planValue p1 seconds + planValue p2 seconds

--bestDoublePlan :: Cave -> Int -> Maybe (Tuple Plan Plan)
--bestDoublePlan cave seconds = bestMovesForAll cave # go "AA" []
--  where
--  go node plan moves = do
--    myMoves <- Map.lookup node moves
--    let
--      updatedMoves = removeDestination moves node
--      withoutMe = removeUsed plan updatedMoves
--      comparePlan best move = do
--        let newPlan = plan <> move
--        if Array.length newPlan > seconds then pure best
--        else do
--          dest <- destination move
--          destBest <- go dest newPlan updatedMoves
--          pure $ if ((planPairValue best seconds) < (planPairValue destBest seconds)) then destBest else best
--    best2 <- bestPlanWithMoves seconds withoutMe
--    foldM comparePlan (plan /\ best2) myMoves

product :: Array Plan -> Array Plan -> Array (Tuple Plan Plan)
product ap1 ap2 = do
  p1 <- ap1
  p2 <- ap2
  if p1 == p2 then []
  else if destination p1 == destination p2 then []
  else [ Tuple p1 p2 ]

bestDoublePlan :: Cave -> Int -> Maybe (Tuple Plan Plan)
bestDoublePlan cave seconds = bestMovesForAll cave # go ("AA" /\ "AA") ([] /\ [])
  where
  go (n1 /\ n2) plan@(p1 /\ p2) moves = do
    let updatedMoves = removeDestination moves n1 # flip removeDestination n2
    n1Moves <- Map.lookup n1 updatedMoves
    n2Moves <- Map.lookup n2 updatedMoves

    let
      movePairs = product n1Moves n2Moves
      comparePlan best@(b1 /\ b2) (m1 /\ m2) = do
        let
          newPlan1 = case p1 <> m1 of
            a
              | Array.length a > seconds -> b1
              | otherwise -> a
          newPlan2 = case p2 <> m2 of
            a
              | Array.length a > seconds -> b2
              | otherwise -> a
        dest1 <- destination newPlan1
        dest2 <- destination newPlan2
        if dest1 == n1 && dest2 == n2 then pure best
        else do
          destBest <- go (dest1 /\ dest2) (newPlan1 /\ newPlan2) updatedMoves
          pure $ if ((planPairValue best seconds) < (planPairValue destBest seconds)) then destBest else best
    foldM comparePlan plan movePairs

main :: Effect Unit
main = do
  --logShow $ parseInput Day16Input.realInput
  --logShow $ planResult samplePlan 30
  --logShow $ parseInput Day16Input.testInput >>= flip bestPlan 30 <#> spy "best" <#> flip planValue 30
  --logShow $ parseInput Day16Input.realInput >>= flip bestPlan 30 <#> spy "best" <#> flip planValue 30
  --logShow $ parseInput Day16Input.testInput >>= flip bestDoublePlan 26 <#> spy "best" <#> flip planPairValue 26
  logShow $ parseInput Day16Input.realInput >>= flip bestDoublePlan 26 <#> spy "best" <#> flip planPairValue 26
