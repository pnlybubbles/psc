module Main where

import Prelude

import Data.Array (head)
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (log)
import Fundamental (Parser, char, many, oneOf, parse, (<|>))

-- AST
data Term = Term Number
data Op = Add | Sub
data Expr' = Infix Op Term Expr' | Phi
data Expr = Expr Term Expr'

derive instance eqTerm :: Eq Term
derive instance eqOp :: Eq Op
derive instance eqExpr' :: Eq Expr'
derive instance eqExpr :: Eq Expr

instance showTerm :: Show Term where
  show (Term n) = showAST "term" [show n]

instance showOp :: Show Op where
  show Add = showAST "+" []
  show Sub = showAST "-" []

instance showExpr' :: Show Expr' where
  show (Infix op t e') = showAST "expr'" [show t, show e']
  show Phi = showAST "phi" []

instance showExpr :: Show Expr where
  show (Expr t e') = showAST "expr" [show t, show e']

showAST :: String -> Array String -> String
showAST label children = label <> "[" <> joinWith " " children <> "]"

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
  t <-term
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

main :: Effect Unit
main = do
  let input = "2+3-4+5"
  log $ "input: " <> input
  log $ show $ head $ _.value <$> parse program input

-- main :: String -> String
-- main value = """.intel_syntax noprefix
-- .global _main
-- _main:
--   mov rax, """ <> src <> """
--   ret
-- """
--   where
--     src = trim value
