module Day11 where

-- {{{ Imports

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((!!))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (foldM, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Day11Input as Day11Input
import Effect (Effect)
import Effect.Console (logShow)

-- }}}

type Monkey =
  { items :: Array BigInt
  , op :: Op
  , test :: Test
  , inspects :: BigInt
  }

data Op = Op OpFunc OpVal
data OpVal
  = Old
  | Literal BigInt

derive instance Generic Op _
instance Show Op where
  show = genericShow

data OpFunc = OpFunc (BigInt -> BigInt -> BigInt)

instance Show OpFunc where
  show (OpFunc f)
    | f (BigInt.fromInt 1) (BigInt.fromInt 2) == (BigInt.fromInt 3) = "+"
    | f (BigInt.fromInt 1) (BigInt.fromInt 2) == (BigInt.fromInt 2) = "*"
    | otherwise = "?"

derive instance Generic OpVal _
instance Show OpVal where
  show = genericShow

data Test = Test BigInt Int Int

derive instance Generic Test _
instance Show Test where
  show = genericShow

parseOp :: String -> Maybe Op
parseOp input = case String.split (Pattern " ") input of
  [ o, r ] -> do
    func <- case o of
      "+" -> Just $ OpFunc (+)
      "*" -> Just $ OpFunc (*)
      _ -> Nothing
    rhs <- case r of
      "old" -> Just Old
      i -> BigInt.fromString i <#> Literal
    Just $ Op func rhs
  _ -> Nothing

-- |Monkey 1:
-- |  Starting items: 54, 65, 75, 74
-- |  Operation: new = old + 6
-- |  Test: divisible by 19
-- |    If true: throw to monkey 2
-- |    If false: throw to monkey 0
parseMonkey :: Array String -> Maybe Monkey
parseMonkey [ _, items', operation, test', target1, target2 ] = do
  --idx <- String.stripPrefix (Pattern "Monkey ") header >>= String.stripSuffix (Pattern ":") >>= Int.fromString
  items <- String.stripPrefix (Pattern "  Starting items: ") items' <#> String.split (Pattern ", ") >>= traverse BigInt.fromString
  op <- String.stripPrefix (Pattern "  Operation: new = old ") operation >>= parseOp
  divBy <- String.stripPrefix (Pattern "  Test: divisible by ") test' >>= BigInt.fromString
  t1idx <- String.stripPrefix (Pattern "    If true: throw to monkey ") target1 >>= Int.fromString
  t2idx <- String.stripPrefix (Pattern "    If false: throw to monkey ") target2 >>= Int.fromString
  let test = Test divBy t1idx t2idx
  Just { items, op, test, inspects: BigInt.fromInt 0 }
parseMonkey _ = Nothing

updateItem :: Op -> BigInt -> BigInt
--updateItem (Op (OpFunc f) Old) i = (f i i) / (BigInt.fromInt 3)
--updateItem (Op (OpFunc f) (Literal j)) i = (f i j) / (BigInt.fromInt 3)
updateItem (Op (OpFunc f) Old) i = (f i i) `mod` (BigInt.fromInt 9699690)
updateItem (Op (OpFunc f) (Literal j)) i = (f i j) `mod` (BigInt.fromInt 9699690)

throwItem :: Test -> Array Monkey -> BigInt -> Maybe (Array Monkey)
throwItem (Test divBy t1 t2) monkeys item = do
  let idx = if item `mod` divBy == (BigInt.fromInt 0) then t1 else t2
  Array.modifyAt idx (\m@{ items } -> m { items = Array.snoc items item }) monkeys

tamper :: Array Monkey -> Int -> Maybe (Array Monkey)
tamper monkeys index = do
  monkey@{ items, op, test, inspects } <- monkeys !! index
  let
    newItems = items <#> updateItem op
    numItems = Array.length items
  newMonkeys <- foldM (throwItem test) monkeys newItems

  let newMonkey = monkey { items = [], inspects = inspects + (BigInt.fromInt numItems) }
  Array.updateAt index newMonkey newMonkeys

runRound :: Array Monkey -> Maybe (Array Monkey)
runRound monkeys = Array.range 0 ((Array.length monkeys) - 1) # foldM tamper monkeys

runRounds :: Int -> Array Monkey -> Maybe (Array Monkey)
runRounds count monkeys = tailRecM go { m: monkeys, c: count }
  where
  go { m, c: 0 } = Just (Done m)
  go { m, c } = runRound m <#> \m' -> Loop { m: m', c: c - 1 }

parseInput :: String -> Maybe (Array Monkey)
parseInput = String.split (Pattern "\n\n") >>> map (String.split (Pattern "\n")) >>> traverse parseMonkey

solve :: String -> Maybe BigInt
solve = parseInput >=> runRounds 10000 >>> map (Array.sortWith _.inspects) >>> map (Array.takeEnd 2) >>> map (foldl (\a { inspects } -> a * inspects) (BigInt.fromInt 1))

main :: Effect Unit
main = do
  logShow $ solve Day11Input.realInput
