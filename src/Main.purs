module Main where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.String (trim)
import Fundamental (Parser, bind', char, many, oneOf, return, (<|>))

-- AST
type Token = Number
data Op = Add | Sub
data Expr' = Infix Op Token Expr' | Phi
data Expr = Expr Token Expr'
type AST = Expr

-- '+'
opAdd :: Parser Op
opAdd = char '+' `bind'` \_ -> return Add
-- '-'
opSub :: Parser Op
opSub = char '-' `bind'` \_ -> return Sub
-- opA = '+' | '-'
opAddSub :: Parser Op
opAddSub = opAdd <|> opSub

-- digit = '0' | '1' | ... | '9'
digit :: Parser Int
digit = oneOf "0123456789"
  `bind'` \c -> return $ toCharCode c - toCharCode '0'

-- number = { digit }
number :: Parser Number
number = (many $ digit)
  `bind'` \ds -> return $ toNumber $ foldl toInt 0 ds
  where
    toInt = \acc x -> x + acc * 10

-- E = T E'
expr :: Parser Expr
expr = number
  `bind'` \t -> expr'
  `bind'` \e' -> return $ Expr t e'

-- E' = opA T E' | e
expr' :: Parser Expr'
expr' = (opAddSub
  `bind'` \op -> number
  `bind'` \t -> expr
  `bind'` \e -> return $ Infix op e)
  <|> return identity

main :: String -> String
main value = """.intel_syntax noprefix
.global _main
_main:
  mov rax, """ <> src <> """
  ret
"""
  where
    src = trim value
