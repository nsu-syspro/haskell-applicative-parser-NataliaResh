{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative ( Alternative(empty, (<|>)) )
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse pa = runParser pa . Position 0

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe pa s = case parse pa s of
  Parsed a _ -> Just a
  _          -> Nothing

instance Functor Parsed where
  fmap f (Parsed a i) = Parsed (f a) i
  fmap _ (Failed xs) = Failed xs

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser rp) = Parser (fmap f . rp)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (Parsed a)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser rf) <*> (Parser rx) = Parser (\i ->
    case rf i of
      Parsed f ii -> fmap f (rx ii)
      Failed xs   -> Failed xs
    )

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\_ -> Failed [])
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser rf <|> Parser rg = Parser (\i ->
    case rf i of
      Failed xs   ->
        case rg i of
          Parsed g j -> Parsed g j
          Failed ys  -> Failed $ nub (xs ++ ys)
      parsed -> parsed
    )

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser (\(Position p s) ->
  case s of
    [] -> Failed [Position p EndOfInput]
    (x:xs)
      | f x       -> Parsed x (Position (p + 1) xs)
      | otherwise -> Failed [Position p (Unexpected x)]
    )
