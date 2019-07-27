module Main where

import Prelude

import Data.Array (concat, head)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), trim)
import Data.String.CodeUnits (indexOf, singleton, uncons)
import Effect (Effect)
import Effect.Console (logShow)

type ParserContinue a = Array { value :: a, rest :: String }
type ParserOp a = String -> ParserContinue a
data Parser a = Parser (ParserOp a)

parse :: forall a. Parser a -> ParserOp a
parse (Parser p) = p

item :: Parser Char
item = Parser $ \cs -> case uncons cs of
  Nothing -> []
  Just { head: c, tail: cs' } -> [{ value: c, rest: cs'}]

return :: forall a. a -> Parser a
return v = Parser $ \cs -> [{ value: v, rest: cs }]

bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \cs ->
  let
    l = parse p cs
    ll = map func l
  in
    concat ll
  where
    func { value: v, rest: s } = parse (f v) s

mzero :: forall a. Parser a
mzero = Parser $ \cs -> []

mplus :: forall a. Parser a -> Parser a -> Parser a
mplus p q = Parser $ \cs -> parse p cs <> parse q cs

choice :: forall a. Parser a -> Parser a -> Parser a
choice p q = Parser $ \cs -> case head $ parse (mplus p q) cs of
  Nothing -> []
  Just x -> [x]

infixr 5 choice as <|>

satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = item `bind` \a ->
  if f a
    then return a
    else mzero

char :: Char -> Parser Char
char a = satisfy $ (==) a

string :: String -> Parser String
string cs = case uncons cs of
  Nothing -> return ""
  Just { head: c, tail: cs' } -> char c
    `bind` \_ -> string cs'
    `bind` \_ -> return cs

oneOf :: String -> Parser Char
oneOf cs = satisfy $ \c -> case indexOf (Pattern $ singleton c) cs of
  Nothing -> false
  Just _ -> true

-- main :: String -> String
-- main value = """.intel_syntax noprefix
-- .global _main
-- _main:
--         mov rax, """ <> trim value <> """
--         ret
-- """
