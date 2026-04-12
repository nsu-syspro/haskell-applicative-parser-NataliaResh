{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser ( Parser, satisfy )
import Data.Char (isDigit)
import ParserCombinators ( char, choice, string )
import Control.Applicative ( Alternative((<|>)) )
import Task1 ( nat )
import Data.List (singleton)
import Data.Functor ( ($>) )

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = choice [dotFormat, hyphenFormat, usFormat]

dotFormat :: Parser Date
dotFormat = dateWithSeparator '.'

hyphenFormat :: Parser Date
hyphenFormat = dateWithSeparator '-'

dateWithSeparator :: Char -> Parser Date
dateWithSeparator s = Date
  <$> day <* char s
  <*> month <* char s
  <*> year

dateFromUsFormat :: Month -> Day -> Year -> Date
dateFromUsFormat m d = Date d m

usFormat :: Parser Date
usFormat = dateFromUsFormat
  <$> monthName <* char ' '
  <*> usDay <* char ' '
  <*> year

dayAfterNine :: [Parser String]
dayAfterNine = [
    sequenceA [char '1' <|> char '2', digit],
    string "30",
    string "31"
  ]

toDay :: Parser String -> Parser Day
toDay = fmap $ Day . read

usDay :: Parser Day
usDay = toDay $ choice (dayAfterNine ++ [singleton <$> nonZeroDigit])

day :: Parser Day
day = toDay $ choice $ sequenceA [char '0', nonZeroDigit] : dayAfterNine

month :: Parser Month
month = Month . read <$> choice [
    sequenceA [char '0', nonZeroDigit],
    string "10",
    string "11",
    string "12"
  ]

monthName :: Parser Month
monthName = choice [
    string "Jan" $> Month 1,
    string "Feb" $> Month 2,
    string "Mar" $> Month 3,
    string "Apr" $> Month 4,
    string "May" $> Month 5,
    string "Jun" $> Month 6,
    string "Jul" $> Month 7,
    string "Aug" $> Month 8,
    string "Sep" $> Month 9,
    string "Oct" $> Month 10,
    string "Nov" $> Month 11,
    string "Dec" $> Month 12
  ]

year :: Parser Year
year = Year . fromIntegral <$> nat

digit :: Parser Char
digit = satisfy isDigit

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\x -> isDigit x && (x /= '0'))
