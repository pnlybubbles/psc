module Main where

import Prelude

import Data.Array (concat, foldl, head)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, singleton, uncons)

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

infixr 4 choice as <|>

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

many :: forall a. Parser a -> Parser (Array a)
many p = p
  `bind` \a -> (many p <|> return [])
  `bind` \as -> return $ [a] <> as

-- '+'
opAdd :: Parser (Number -> Number -> Number)
opAdd = char '+' `bind` \_ -> return (+)
-- '-'
opSub :: Parser (Number -> Number -> Number)
opSub = char '-' `bind` \_ -> return (-)
-- opA = '+' | '-'
opAddSub :: Parser (Number -> Number -> Number)
opAddSub = opAdd <|> opSub

-- '*'
opMul :: Parser (Number -> Number -> Number)
opMul = char '*' `bind` \_ -> return (*)
-- '/'
opDiv :: Parser (Number -> Number -> Number)
opDiv = char '/' `bind` \_ -> return (/)
-- opM = '*' | '/'
opMulDiv :: Parser (Number -> Number -> Number)
opMulDiv = opMul <|> opDiv

-- digit = '0' | '1' | ... | '9'
digit :: Parser Int
digit = oneOf "0123456789"
  `bind` \c -> return $ toCharCode c - toCharCode '0'

-- number = { digit }
number :: Parser Number
number = (many $ digit)
  `bind` \ds -> return $ toNumber $ foldl toInt 0 ds
  where
    toInt = \acc x -> x + acc * 10

-- F = number | '(' expr ')'
factor :: Parser Number
factor = number

-- T = T opM F | F
-- T = F T'
term :: Parser Number
term = factor
  `bind` \f -> term'
  `bind` \op -> return $ op f

-- T' = opM F T' | e
term' :: Parser (Number -> Number)
term' = (opMulDiv
  `bind` \bop -> factor
  `bind` \f -> term'
  `bind` \op -> return (flip bop f >>> op))
  <|> return identity

-- main :: String -> String
-- main value = """.intel_syntax noprefix
-- .global _main
-- _main:
--         mov rax, """ <> trim value <> """
--         ret
-- """
