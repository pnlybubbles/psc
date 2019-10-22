module Parser where

import Prelude

import Ast (Expr(..), Expr'(..), Op(..), Term(..))
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Fundamental (Parser, char, many, oneOf, (<|>))

-- '+'
opAdd :: Parser Op
opAdd = do
  _ <- char '+'
  pure Add

-- '-'
opSub :: Parser Op
opSub = do
  _ <- char '-'
  pure Sub

-- opA = '+' | '-'
opAddSub :: Parser Op
opAddSub = opAdd <|> opSub

-- digit = '0' | '1' | ... | '9'
digit :: Parser Int
digit = do
  c <- oneOf "0123456789"
  pure $ toCharCode c - toCharCode '0'

-- number = { digit }
number :: Parser Number
number = do
  ds <- (many $ digit)
  pure $ toNumber $ foldl toInt 0 ds
  where
    toInt = \acc x -> x + acc * 10

-- term = number
term :: Parser Term
term = do
  n <- number
  pure $ Term n

-- E = T E'
expr :: Parser Expr
expr = do
  t <- term
  e' <- expr'
  pure $ Expr t e'

-- E' = opA T E' | e
expr' :: Parser Expr'
expr' = do
  op <- opAddSub
  t <- term
  e' <- expr'
  pure $ Infix op t e'
  <|> pure Phi

program :: Parser Expr
program = expr
