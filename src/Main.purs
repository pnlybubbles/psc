module Main where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Fundamental (Parser, bind', char, many, oneOf, return, (<|>))

-- '+'
opAdd :: Parser (Number -> Number -> Number)
opAdd = char '+' `bind'` \_ -> return (+)
-- '-'
opSub :: Parser (Number -> Number -> Number)
opSub = char '-' `bind'` \_ -> return (-)
-- opA = '+' | '-'
opAddSub :: Parser (Number -> Number -> Number)
opAddSub = opAdd <|> opSub

-- '*'
opMul :: Parser (Number -> Number -> Number)
opMul = char '*' `bind'` \_ -> return (*)
-- '/'
opDiv :: Parser (Number -> Number -> Number)
opDiv = char '/' `bind'` \_ -> return (/)
-- opM = '*' | '/'
opMulDiv :: Parser (Number -> Number -> Number)
opMulDiv = opMul <|> opDiv

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

-- F = number | '(' expr ')'
factor :: Parser Number
factor = number

-- T = T opM F | F
-- T = F T'
term :: Parser Number
term = factor
  `bind'` \f -> term'
  `bind'` \op -> return $ op f

-- T' = opM F T' | e
term' :: Parser (Number -> Number)
term' = (opMulDiv
  `bind'` \bop -> factor
  `bind'` \f -> term'
  `bind'` \op -> return (flip bop f >>> op))
  <|> return identity

-- E = T E'
expr :: Parser Number
expr = term
  `bind'` \t -> expr'
  `bind'` \op -> return $ op t

-- E' = opA T E' | e
expr' :: Parser (Number -> Number)
expr' = (opAddSub
  `bind'` \bop -> term
  `bind'` \t -> expr'
  `bind'` \op -> return (flip bop t >>> op))
  <|> return identity

-- main :: String -> String
-- main value = """.intel_syntax noprefix
-- .global _main
-- _main:
--         mov rax, """ <> trim value <> """
--         ret
-- """
