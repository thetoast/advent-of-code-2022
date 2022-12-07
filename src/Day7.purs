module Day7 where

-- {{{ Imports

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, sum)
import Data.Int (fromString)
import Data.List (reverse, sort)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Day7Input as Day7Input
import Debug (spy)
import Effect (Effect)
import Effect.Console (log, logShow)

-- }}}

data Command
  = UP
  | CD String
  | LS (Array LSResult)

data LSResult
  = File { name :: String, size :: Int }
  | Folder { name :: String }

newtype Name = Name String

derive instance Eq Name
derive instance Ord Name
instance Show Name where
  show (Name n) = n

newtype FQName = FQName String

derive instance Eq FQName
derive instance Ord FQName
instance Show FQName where
  show (FQName n) = n

type State =
  { children :: Map FQName (Array Name)
  , size :: Map FQName Int
  , parent :: Map FQName FQName
  , curr :: FQName
  }

parseCommand :: String -> Maybe Command
parseCommand input = case String.split (Pattern " ") input of
  [ "$", "cd", ".." ] -> Just UP
  [ "$", "cd", folder ] -> Just $ CD folder
  [ "$", "ls" ] -> Just $ LS []
  _ -> Nothing

getThing :: String
getThing = "not used"

--getChildren :: State -> Maybe (Array FSEntry)
--getChildren state = do
--  case Array.last state of
--    Just (Folder { entries }) -> Just entries
--    _ -> Nothing

path :: FQName -> Name -> FQName
path (FQName "/") (Name child) = FQName $ "/" <> child
path (FQName parent) (Name child) = FQName $ parent <> "/" <> child

getChild :: State -> Name -> Maybe FQName
getChild state child = Map.lookup state.curr state.children >>= Array.find (eq child) <#> path state.curr

getParent :: State -> Maybe FQName
getParent state = Map.lookup state.curr state.parent

lsName :: LSResult -> Name
lsName (File f) = (Name f.name)
lsName (Folder f) = (Name f.name)

handleCommand :: State -> Command -> Maybe State
handleCommand state UP = case state.curr of
  (FQName "/") -> Just state
  _ -> getParent state <#> \p -> state { curr = p }

handleCommand state (CD "/") = Just state { curr = (FQName "/") }

handleCommand state (CD dir) = do
  child <- getChild state (Name dir)
  Just state { curr = child }

handleCommand state@{ curr } (LS results) = Just $ foldl addResult state results
  where
  addResult :: State -> LSResult -> State
  addResult s@{ children, parent, size } res =
    let
      name = lsName res
      newChildren = Map.insertWith (<>) curr [ name ] children
      newParent = Map.insert (path curr name) curr parent
    in
      case res of
        File f -> s { children = newChildren, parent = newParent, size = Map.insert (path curr (Name f.name)) f.size size }
        Folder _ -> s { children = newChildren, parent = newParent }

loadLS :: Array String -> Maybe { ls :: Command, rest :: Array String }
loadLS lines = loadLines [] lines <#> \{ res, lines' } -> { ls: LS res, rest: lines' }
  where
  loadLines res [] = Just { res, lines': [] }
  loadLines res lines' = do
    { head, tail } <- Array.uncons lines'
    case String.split (Pattern " ") head of
      [ "$", _, _ ] -> Just { res, lines' }
      [ "dir", name ] -> loadLines (Array.snoc res (Folder { name })) tail
      [ s, name ] -> fromString s >>= \size -> loadLines (Array.snoc res (File { size, name })) tail
      _ -> Nothing

parseInput :: String -> Maybe State
parseInput input = parseLines { curr: (FQName "/"), children: Map.empty, parent: Map.empty, size: Map.empty } (String.split (Pattern "\n") input)
  where
  parseLines state [] = Just state
  parseLines state lines = do
    { head, tail } <- Array.uncons lines
    command <- parseCommand head
    case command of
      LS [] -> do
        { ls, rest } <- loadLS tail
        newState <- handleCommand state ls
        parseLines newState rest
      c -> handleCommand state c >>= \s -> parseLines s tail

showTree :: State -> FQName -> String
showTree state@{ children, size } dir =
  let
    header = show dir <> " (dir)"
    childs = path dir <$> case Map.lookup dir children of
      Just c -> c
      Nothing -> []
    childShow = childs <#> \c -> case Map.lookup c size of
      Just s -> show c <> " (file, size=" <> show s <> ")"
      _ -> showTree state c
  in
    Array.intercalate "\n" (Array.cons header childShow)

sizeOfNode :: State -> FQName -> Int
sizeOfNode state@{ children, size } p =
  case Map.lookup p size of
    Just s -> s
    Nothing ->
      let
        childs = path p <$> case Map.lookup p children of
          Just c -> c
          Nothing -> []
      in
        sum $ childs <#> sizeOfNode state

dirSizes :: State -> FQName -> { dirSizes :: Map FQName Int, totalSize :: Int }
dirSizes state@{ children, size } p =
  case Map.lookup p size of
    Just s -> { dirSizes: Map.empty, totalSize: s }
    Nothing ->
      let
          childs = path p <$> case Map.lookup p children of
                                  Just c -> c
                                  Nothing -> []
          total = childs <#> dirSizes state # foldl (\a c -> {dirSizes: Map.union a.dirSizes c.dirSizes, totalSize: a.totalSize + c.totalSize}) {dirSizes: Map.empty, totalSize: 0}
       in total{ dirSizes = Map.insert p total.totalSize total.dirSizes}

solve1 :: State -> Int -> Int
solve1 state maxSize =
  let
      sizes = dirSizes state (FQName "/") # _.dirSizes
      filtered = Map.toUnfoldable sizes # Array.mapMaybe \(Tuple _ v) -> if v <= maxSize then Just v else Nothing
   in sum filtered

solve2 :: State -> Int -> Int -> Maybe Int
solve2 state totalSize targetFree = do
  let sizes = dirSizes state (FQName "/") # _.dirSizes
  let cs = Map.values sizes
  available <- Map.lookup (FQName "/") sizes <#> \s -> totalSize - s
  Just $ foldl (\a c -> if available + c >= targetFree && c < a then c else a) totalSize cs

main :: Effect Unit
main = do
  case parseInput Day7Input.realInput of
    Just s -> do
      --log $ showTree s (FQName "/")
      logShow $ solve1 s 100000
      logShow $ solve2 s 70000000 30000000
    Nothing -> log "no bueno"
