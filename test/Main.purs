module Test.Main where

import Prelude

import Data.Array (head)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Main (Parser, ParserContinue, bind, char, item, mplus, mzero, oneOf, parse, return, satisfy, string, (<|>))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

assertParser :: forall a. Eq a => ParserContinue a -> a -> String -> Aff Unit
assertParser r value rest = do
  Assert.assert "length" $ Array.length r == 1
  Assert.assert "value" $ (head r <#> \v -> v.value) == Just value
  Assert.assert "rest" $ (head r <#> \v -> v.rest) == Just rest

main :: Effect Unit
main = runTest do

  suite "Parser Monad" do
    test "return" do
      let r = parse (return "hello") "world"
      assertParser r "hello" "world"

    -- return a >>= f == f a
    -- 交換法則 a * f == f * a
    test "Left Identity" do
      let
        a = "hello"
        f = \x -> return $ x <> "world"
        p1 = return a `bind` f
        p2 = f a
        r1 = parse p1 "test"
        r2 = parse p2 "test"
      assertParser r1 "helloworld" "test"
      Assert.assert "equals" $ r1 == r2

    -- m >>= return == m
    -- 単位元 m * I = m
    test "Right Identity" do
      let
        m = return "hello"
        p1 = m `bind` return
        p2 = m
        r1 = parse p1 "test"
        r2 = parse p2 "test"
      assertParser r1 "hello" "test"
      Assert.assert "equals" $ r1 == r2

    -- (m >>= f) >>= g == m >>= (\x -> f x >>= g)
    -- 結合法則 (m * f) * g = m * (f * g)
    test "Associativity" do
      let
        m = return "hello"
        f = \x -> return $ x <> "world"
        g = \x -> return $ String.length x
        p1 = (m `bind` f) `bind` g
        p2 = m `bind` (\x -> f x `bind` g)
        r1 = parse p1 "test"
        r2 = parse p2 "test"
      assertParser r1 10 "test"
      Assert.assert "equals" $ r1 == r2

  suite "MonadPlus" do
    test "mzero" do
      let r = parse mzero "test"
      Assert.assert "length" $ Array.length r == 0

    test "mplus" do
      let
        m = return "hello"
        n = return "world"
        p = m `mplus` n
        r = parse p "test"
      Assert.assert "length" $ Array.length r == 2

    test "choice" do
      let
        m = return "hello"
        n = return "world"
        p = m <|> n
        r = parse p "test"
      assertParser r "hello" "test"

  suite "Match to /./" do
    test "Parse 'a' to get any character and '' is left" do
      let r = parse item "a"
      assertParser r 'a' ""

    test "Parse 'abc123' to get any character and 'bc123' is left" do
      let r = parse item "abc123"
      assertParser r 'a' "bc123"

    test "Parse '' to get any character and reject" do
      let r = parse item ""
      Assert.assert "length" $ Array.length r == 0

    test "Parse 'abc123' to get 2 characters and 'c123' is left" do
      let
        p = item
          `bind` \a -> item
          `bind` \b -> return $ fromCharArray [a, b]
        r = parse p "abc123"
      assertParser r "ab" "c123"

    test "Parse 'a' to get 2 characters and reject" do
      let
        p = item
          `bind` \a -> item
          `bind` \b -> return $ fromCharArray [a, b]
        r = parse p "a"
      Assert.assert "length" $ Array.length r == 0

  suite "Match to the character which passes the provided function" do
    test "Fulfill" do
      let
        p = satisfy \x -> x == 'a'
        r = parse p "a"
      assertParser r 'a' ""

    test "Reject" do
      let
        p = satisfy \x -> x == 'a'
        r = parse p "b"
      Assert.assert "length" $ Array.length r == 0

  suite "Match to /a/" do
    test "Parse 'a' to get 'a' and consume character" do
      let
        p = char 'a'
        r = parse p "a"
      assertParser r 'a' ""

    test "Parse 'b' to get 'a' and reject" do
      let
        p = char 'a'
        r = parse p "b"
      Assert.assert "length" $ Array.length r == 0

  suite "Match to /aaa/" do
    test "Parse 'abc123' to get 'abc' and '123' is left" do
      let
        p = string "abc"
        r = parse p "abc123"
      assertParser r "abc" "123"

    test "Parse 'aaa123' to get 'abc' and reject" do
      let
        p = string "abc"
        r = parse p "aaa123"
      Assert.assert "length" $ Array.length r == 0

  suite "Match to /aaa|bbb|ccc/" do
    test "Parse 'baz123' to get 'foo' or 'baz'" do
      let
        p = string "foo" <|> string "baz"
        r = parse p "baz123"
      assertParser r "baz" "123"

    test "Parse 'baz123' to get 'foo' or 'bar' or 'baz'" do
      let
        p = string "foo" <|> string "bar" <|> string "baz"
        r = parse p "baz123"
      assertParser r "baz" "123"

    test "Parse 'xxx123' to get 'foo' or 'bar' or 'baz' and reject" do
      let
        p = string "foo" <|> string "bar" <|> string "baz"
        r = parse p "xxx123"
      Assert.assert "length" $ Array.length r == 0
