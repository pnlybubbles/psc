module Main where

import Prelude
import Data.String (trim)

main :: String -> String
main value = """.intel_syntax noprefix
.global _main
_main:
        mov rax, """ <> trim value <> """
        ret
"""
