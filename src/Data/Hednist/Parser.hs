{-# LANGUAGE  FlexibleContexts #-}
module Data.Hednist.Parser (
  nil,
  bool,
  character,
  unicode,
  symbol,
  keyword,
  int,
  str,
  integer,
  float,
  list,
  vector,
  dict,
  tagged,
  edn,
  edns
  ) where

import Control.Applicative
import Control.Monad
import Data.Char (chr)
import qualified Data.Set as S
import Numeric
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim hiding ((<|>), many)

import Data.Hednist.Types

nil :: Parser EDNValue
nil = string "nil" *> pure EDNNil

bool :: Parser EDNValue
bool = (true *> pure (EDNBool True)) <|> (false *> pure (EDNBool False))
  where true  = string "true"
        false = string "false"

unicode :: Parser Char
unicode = char 'u' *> (decode <$> hexDigit <*> hexDigit <*> hexDigit <*> hexDigit)
  where decode :: Char -> Char -> Char -> Char -> Char
        decode a b c d = chr . fst . head . readHex $ [a, b, c, d]

character :: Parser EDNValue
character =
    backslash *>
      (try lf
       <|> try cr
       <|> try space
       <|> try tab
       <|> try (fmap EDNChar unicode)
       <|> EDNChar <$> anyChar)
  where lf        = string "newline" *> pure (EDNChar '\n')
        cr        = string "return" *> pure (EDNChar '\r')
        space     = string "space" *> pure (EDNChar ' ')
        tab       = string "tab" *> pure (EDNChar '\t')
        backslash = char '\\' *> pure ()

-- Components of a symbol
sym :: Parser String
sym = special <|> standard
  where special  = composite
                    <$> fmap (:[]) (oneOf  "+-.")
                    <*> many1 (oneOf ".*+!-_?$%&=<>:#" <|> letter)
                    <*> many symChar
        standard = (++) <$> many1 symStart <*> many symChar
        symStart = oneOf "*!_?$%&=<>" <|> letter -- Letters that may start a symbol
        symChar  = oneOf ".*+!-_?$%&=<>:#" <|> alphaNum -- Letters that may appear anywhere in a symbol
        composite a b c = a ++ b ++ c

-- Note: There is a need for backtracking interaction between symbol and int due
-- to the ability to place a + or - at the start
symbol :: Parser EDNValue
symbol = string "/" *> pure (EDNSymbol Nothing "/")
    <|> try (EDNSymbol <$> (Just  <$> sym) <*> (string "/" *> sym))
    <|> EDNSymbol Nothing <$> sym

keyword :: Parser EDNValue
keyword = string ":" *>
    (try (EDNKeyword <$> (Just <$> sym) <*> (string "/" *> sym))
    <|> EDNKeyword Nothing <$> sym)

int :: Parser String
int = pos <|> neg <|> unprefixed
  where pos :: Parser String
        pos = char '+' *> unprefixed
        neg :: Parser String
        neg = (:) <$> char '-' <*> unprefixed
        unprefixed :: Parser String
        unprefixed = ((:) <$> nonzero <*> many digit) <|> ((:[]) <$> digit)
        nonzero :: Parser Char
        nonzero = oneOf "123456789"

integer :: Parser EDNValue
integer = try arbint <|> fixedint
  where arbint :: Parser EDNValue
        arbint = ((EDNInteger . read) <$> int) <* char 'N'
        fixedint :: Parser EDNValue
        fixedint = EDNInt . read <$> int

-- TOOD: Introduce arbitrary precision via numbers
float :: Parser EDNValue
float = try coerced <|> try power <|> try fp
  where coerced :: Parser EDNValue
        coerced = ((EDNFloat . read) <$> int) <* char 'M'
        fp :: Parser EDNValue
        fp = (EDNFloat . read) <$> decimal
        power :: Parser EDNValue
        power = readExponent <$> (try decimal <|> int) <*> (e *> int)
        decimal :: Parser String
        decimal = (++) <$> int <*> frac
        -- Utility string parsers
        frac :: Parser String
        frac = (:) <$> char '.' <*> many1 digit
        e :: Parser ()
        e = (char 'e' <|> char 'E') *> pure ()
        readExponent :: String -> String -> EDNValue
        readExponent base exp = EDNFloat $ read base * (10 ** read exp)

str :: Parser EDNValue
str = char '"' *> content <* char '"'
  where content :: Parser EDNValue
        content = EDNString <$> many (escaped <|> noneOf ['"'])
        escaped :: Parser Char
        escaped = try (backslash *> char 'n' *> pure '\n')
                  <|> try (backslash *> char 'r' *> pure '\r')
                  <|> try (backslash *> char 't' *> pure '\t')
                  <|> try (backslash *> char '\\')
                  <|> try (backslash *> char '"')
        backslash :: Parser Char
        backslash = char '\\'

whitespace :: Parser Char
whitespace = space <|> tab <|> newline <|> char '\r'

lexeme :: Parser a -> Parser a
lexeme p = many whitespace *> p <* many whitespace

edns :: Parser [EDNValue]
edns = many edn <* many (whitespace *> pure () <|> comment)

list :: Parser EDNValue
list = char '(' *> (EDNList <$> edns) <* char ')'

vector :: Parser EDNValue
vector = char '[' *> (EDNVector <$> edns) <* char ']'

-- TODO: Ensure set uniqueness
set :: Parser EDNValue
set = string "#{" *> (EDNSet <$> edns) <* char '}'

-- TODO: Consider ways in which we might map from the symbol true -> EDNBool
-- instead of handling parsing separately

entry :: Parser (EDNValue, EDNValue)
entry = (,) <$> lexeme edn <*> lexeme edn

dict :: Parser EDNValue
dict = char '{' *> (EDNMap <$> kvs) <* char '}'
  where kvs :: Parser [(EDNValue, EDNValue)]
        kvs = try ((:) <$> kv <*> option [] (sep *> kvs))
              <|> pure []
        kv :: Parser (EDNValue, EDNValue)
        kv = many whitespace *> ((,) <$> lexeme edn <*> lexeme edn)
        sep :: Parser Char
        sep = char ','

tagged :: Parser EDNValue
tagged = char '#' *> (try (EDNTagged <$> Just <$> sym <*> sym <*> edn)
                      <|> EDNTagged Nothing <$> sym <*> edn)

discard :: Parser EDNValue
discard = string "#_" *> (EDNDiscard <$> edn)

comment :: Parser ()
comment = char ';' *> content
  where content = (char '\n' *> pure ())
                  <|> (noneOf "\n" *> content)

consumeJunk :: Parser a -> Parser a
consumeJunk p = junk *> p
  where junk = many ((whitespace *> pure ()) <|> comment)

edn :: Parser EDNValue
edn = consumeJunk
        (try float
        <|> integer
        <|> try bool -- bool might confuse with a symbol starting with t or f otherwise
        <|> str
        <|> symbol
        <|> keyword
        <|> list
        <|> vector
        <|> try discard
        <|> try set
        <|> try dict -- These 3 elemens all begin with #
        <|> try tagged)
