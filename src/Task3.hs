{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser ( Parser, Parsed(..), parse, satisfy )
import Data.Char (toLower, isDigit, isAscii)
import Data.List (intercalate, singleton)
import ParserCombinators ( char, string, choice )
import Data.Functor ( ($>) )
import Control.Applicative ( Alternative((<|>), many) )
import Task2 ( digit, nonZeroDigit )

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Position 2 "")
-- >>> parse json "null"
-- Parsed JNull (Position 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Position 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Position 4 "")
-- >>> parse json "{{}}"
-- Failed [Position 0 (Unexpected '{'),Position 1 (Unexpected '{')]
--
json :: Parser JValue
json = whitespaces *> value <* whitespaces
  
value :: Parser JValue
value = jNull <|> jBool <|> jObject
        <|> jArray <|> jNumber <|> jString

stringLiteral :: Parser String
stringLiteral = concat <$> (char '"' *> many jChar <* char '"')
  where
    -- >>> parse jChar "\\\""
    -- Parsed "\\\"" (Position 2 "")
    jChar :: Parser String
    jChar =
      sequenceA [char '\\', choiceChars ['\"', '\\', '/', 'b', 'f', 'n', 'r', 't']]
      <|> sequenceA [char '\\', char 'u', hexDigit, hexDigit, hexDigit, hexDigit]
      <|> singleton <$> satisfy (\x -> isAscii x && x /= '"' && x /= '\\')

    hexDigit :: Parser Char
    hexDigit = satisfy isDigit <|> choiceChars (['a'..'f'] ++ ['A'..'F'])

option :: Parser String -> Parser String
option = (<|> pure "")

list :: Parser a -> Parser [a]
list p = ((:) <$> (p <* string ",") <*> list p)
  <|> singleton <$> p

whitespaces :: Parser String
whitespaces = 
  option ((:) <$> choiceChars [' ', '\t', '\n', '\r'] <*> whitespaces)

jObject :: Parser JValue
jObject = 
  JObject <$> (char '{' *> members <* char '}')
  <|> (char '{' *> whitespaces *> char '}' $> JObject [])
  where
    member :: Parser (String, JValue)
    member = (,)
      <$> (whitespaces *> stringLiteral <* whitespaces)
      <*> (string ":" *> json)

    members :: Parser [(String, JValue)]
    members = list member

jArray :: Parser JValue
jArray = 
  JArray <$> (char '[' *> elements <* char ']')
  <|> (char '[' *> whitespaces *> char ']' $> JArray [])
  where
    elements :: Parser [JValue]
    elements = list json

-- >>> parse jString "\"a\\bc\""
-- Parsed (JString "a\\bc") (Position 6 "")
jString :: Parser JValue
jString = JString <$> stringLiteral

choiceChars :: [Char] -> Parser Char
choiceChars = choice . map char

-- >>> parse jNumber "3.0e14"
-- Parsed (JNumber 3.0e14) (Position 6 "")
jNumber :: Parser JValue
jNumber = JNumber . read <$> (
  (++) <$> int <*> ((++) <$> option fraction <*> option expon))
  where
    int :: Parser String
    int =
      ((++) <$> option minus <*> ((:) <$> nonZeroDigit <*> digits))
      <|> ((++) <$> option minus <*> oneDigit)

    digits :: Parser String
    digits = 
      ((:) <$> digit <*> digits)
      <|> oneDigit

    oneDigit = singleton <$> digit

    fraction :: Parser String
    fraction = (:) <$> char '.' <*> digits

    expon :: Parser String
    expon = (:) <$>
        choiceChars ['e', 'E']
        <*> ((++) <$> option sign
        <*> digits)

    minus = string "-"

    sign = minus <|> string "+"

jNull :: Parser JValue
jNull = string "null" $> JNull

jBool :: Parser JValue
jBool = choice [
    string "true"  $> JBool True,
    string "false" $> JBool False
  ]

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
