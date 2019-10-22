module Ast where

import Prelude

import Data.String (joinWith)

-- AST
data Term = Term Number
data Op = Add | Sub
data Expr' = Infix Op Term Expr' | Phi
data Expr = Expr Term Expr'
type AST = Expr

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

