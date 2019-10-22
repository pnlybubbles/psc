module Fundamental where

import Prelude

import Data.Array (concat, head)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf)
import Data.String.CodeUnits (singleton, uncons)

type ParserContinue a = Array { value :: a, rest :: String }
type ParserOp a = String -> ParserContinue a
data Parser a = Parser (ParserOp a)

parse :: forall a. Parser a -> ParserOp a
parse (Parser p) = p

item :: Parser Char
item = Parser $ \cs -> case uncons cs of
  Nothing -> []
  Just { head: c, tail: cs' } -> [{ value: c, rest: cs'}]

instance functorParser :: Functor Parser where
  map f p = Parser $ \cs ->
    let
      l = parse p cs
    in
      map func l
    where
      func { value: v, rest: s } = { value: f v, rest: s }

instance applyParser :: Apply Parser where
  apply pf p = Parser $ \cs ->
    let
      l = parse pf cs
      ll = map func l
    in
      concat ll
    where
      func { value: f, rest: s } = parse (map f p) s

instance applicativeParser :: Applicative Parser where
  pure v = Parser $ \cs -> [{ value: v, rest: cs }]

instance bindParser :: Bind Parser where
  bind p f = Parser $ \cs ->
    let
      l = parse p cs
      ll = map func l
    in
      concat ll
    where
      func { value: v, rest: s } = parse (f v) s

instance monadParser :: Monad Parser

mzero :: forall a. Parser a
mzero = Parser $ \cs -> []

mplus :: forall a. Parser a -> Parser a -> Parser a
mplus p q = Parser $ \cs -> parse p cs <> parse q cs

choice :: forall a. Parser a -> Parser a -> Parser a
choice p q = Parser $ \cs -> case head $ parse (mplus p q) cs of
  Nothing -> []
  Just x -> [x]

infixr 4 choice as <|>

satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = item >>= \a ->
  if f a
    then pure a
    else mzero

char :: Char -> Parser Char
char a = satisfy $ (==) a

string :: String -> Parser String
string cs = case uncons cs of
  Nothing -> pure ""
  Just { head: c, tail: cs' } -> char c
    >>= \_ -> string cs'
    >>= \_ -> pure cs

oneOf :: String -> Parser Char
oneOf cs = satisfy $ \c -> case indexOf (Pattern $ singleton c) cs of
  Nothing -> false
  Just _ -> true

many :: forall a. Parser a -> Parser (Array a)
many p = p
  >>= \a -> (many p <|> pure [])
  >>= \as -> pure $ [a] <> as
