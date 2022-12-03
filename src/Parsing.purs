module Parsing where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

type Parser x = String -> Maybe x

linesFrom :: forall x. Parser x -> String -> Maybe (Array x)
linesFrom parser = split (Pattern "\n") >>> traverse parser

-- | Simple line parser that parses a string of format "xxxx yyyy"
-- | Takes two Parser functions and returns a Tuple of both results if
-- | successful
tupleParser :: forall x y. Parser x -> Parser y -> String -> Maybe (Tuple x y)
tupleParser parseLeft parseRight = tupleParser' parseLeft (\_ -> parseRight)

-- | Simple line parser that parses a string of format "xxxx yyyy"
-- | Takes one Parser function and another function which returns a Parser
-- | based on the value x, then returns a Tuple of both results if successful
-- |
-- | This is useful if the value y is dependent on the value x
tupleParser' :: forall x y. Parser x -> (x -> Parser y) -> String -> Maybe (Tuple x y)
tupleParser' parseLeft parseRight line = do
  let parts = split (Pattern " ") line
  left <- parts !! 0 >>= parseLeft
  right <- parts !! 1 >>= parseRight left
  Just $ Tuple left right
