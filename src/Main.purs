module Main where

import Prelude

import Ast (AST, Expr(..), Expr'(..), Op(..), Term(..))
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.String (trim)
import Fundamental (parse)
import Parser (program)

compile :: AST -> String
compile (Expr (Term n) e') = """.intel_syntax noprefix
.global _main
_main:
""" <> "mov rax, " <> toString n <> "\n" <> compileExpr' e' <> "ret\n"

compileExpr' :: Expr' -> String
compileExpr' Phi = ""
compileExpr' (Infix op (Term n) e') = ops <> " rax, " <> toString n <> "\n" <> compileExpr' e'
  where
    ops = case op of
      Add -> "add"
      Sub -> "sub"

type Result = { result :: String, error :: String }

main :: String -> Result
main value = case ast of
  Nothing -> { result: "", error: "Parse failed" }
  Just a -> { result: compile a, error: "" }
  where
    input = trim value
    ast = head $ _.value <$> parse program input
