module Main where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.String (trim)
import Fundamental (Parser, bind', char, many, oneOf, return, (<|>))

-- AST
data Op = Add | Sub
data Expr = Factor Number | Infix Op Expr Expr

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
expr :: Parser Number
expr = number
  `bind'` \t -> expr'
  `bind'` \op -> return $ op t

-- E' = opA T E' | e
expr' :: Parser ()
expr' = (opAddSub
  `bind'` \op -> number
  `bind'` \t -> expr'
  `bind'` \e -> return $ Infix op t)
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
