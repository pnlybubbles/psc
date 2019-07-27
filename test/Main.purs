module Test.Main where

import Prelude

import Data.Array (head)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Main (ParserContinue, bind, char, digit, factor, item, many, mplus, mzero, number, oneOf, opMulDiv, parse, return, satisfy, string, term, term', (<|>))
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
    let
      p2 = item
        `bind` \a -> item
        `bind` \b -> return $ fromCharArray [a, b]

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
      let r = parse p2 "abc123"
      assertParser r "ab" "c123"

    test "Parse 'a' to get 2 characters and reject" do
      let r = parse p2 "a"
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
    test "Parse 'baz123' to match /foo|baz/" do
      let
        p = string "foo" <|> string "baz"
        r = parse p "baz123"
      assertParser r "baz" "123"

    test "Parse 'baz123' to match /foo|bar|baz/" do
      let
        p = string "foo" <|> string "bar" <|> string "baz"
        r = parse p "baz123"
      assertParser r "baz" "123"

    test "Parse 'xxx123' to match /foo|bar|baz/ and reject" do
      let
        p = string "foo" <|> string "bar" <|> string "baz"
        r = parse p "xxx123"
      Assert.assert "length" $ Array.length r == 0

  suite "Match to /ab?c/" do
    let
      p = char 'a'
        `bind` \a -> pb
        `bind` \b -> char 'c'
        `bind` \c -> return $ fromCharArray $ [a] <> b <> [c]
      pb = (char 'b' `bind` \b -> return [b]) <|> return []

    test "Parse 'ac'" do
      let
        r = parse p "ac"
      assertParser r "ac" ""

    test "Parse 'abc'" do
      let
        r = parse p "abc"
      assertParser r "abc" ""

  suite "Match to /.+/" do
    test "Parse 'aaab' to match /a+/" do
      let
        p = many $ char 'a'
        r = parse p "aaab"
      assertParser r ['a', 'a', 'a'] "b"

    test "Parse 'bbbb' to match /a+/ and reject" do
      let
        p = many $ char 'a'
        r = parse p "bbbb"
      Assert.assert "length" $ Array.length r == 0

  suite "Match to /[oneof]/" do
    test "Parse 'b' to match /[abc]/" do
      let
        p = oneOf "abc"
        r = parse p "b"
      assertParser r 'b' ""

    test "Parse '3' to match /[0-9]/" do
      let r = parse digit "3"
      assertParser r 3 ""

    test "Parse '567' to match /[0-9]/ and '67' is left" do
      let r = parse digit "567"
      assertParser r 5 "67"

  suite "Match to /[0-9]+/" do
    test "Parse '123'" do
      let r = parse number "123"
      assertParser r 123.0 ""

    test "Parse '012'" do
      let r = parse number "012"
      assertParser r 12.0 ""

    test "Parse '12a' and 'a' is left" do
      let r = parse number "12a"
      assertParser r 12.0 "a"

  suite "Match to /[*/]/" do
    let p = opMulDiv `bind` \bop -> return $ 4.0 `bop` 2.0

    test "Parse '*'" do
      let r = parse p "*"
      assertParser r 8.0 ""

    test "Parse '/'" do
      let r = parse p "/"
      assertParser r 2.0 ""

  suite "Match to /[*/][0-9]+/" do
    let
      term1 = opMulDiv
        `bind` \bop -> factor
        `bind` \f -> return $ flip bop f
      p = term1
        `bind` \op -> return $ op 4.0

    test "Parse '*2'" do
      let r = parse p "*2"
      assertParser r 8.0 ""

    test "Parse '*20'" do
      let r = parse p "*20"
      assertParser r 80.0 ""

    test "Parse '/2'" do
      let r = parse p "/2"
      assertParser r 2.0 ""

  suite "Match to /([*/][0-9])?/" do
    let
      term1 = (opMulDiv
        `bind` \bop -> factor
        `bind` \f -> return (flip bop f))
        <|> return identity
      p = term1
        `bind` \op -> return $ op 4.0

    test "Parse ''" do
      let r = parse p ""
      assertParser r 4.0 ""

    test "Parse '*2'" do
      let r = parse p "*2"
      assertParser r 8.0 ""

  suite "Match to /([*/][0-9]+)*/" do
    let p = term' `bind` \op -> return $ op $ 4.0

    test "parse ''" do
      let r = parse p ""
      assertParser r 4.0 ""

    test "Parse '*2'" do
      let r = parse p "*2"
      assertParser r 8.0 ""

    test "Parse '*2*3'" do
      let r = parse p "*2*3"
      assertParser r 24.0 ""

  suite "Match to /[0-9]+([*/][0-9]+)*/" do
    test "Parse '2*3'" do
      let r = parse term "2*3"
      assertParser r 6.0 ""

    test "Parse '3/2'" do
      let r = parse term "3/2"
      assertParser r 1.5 ""

    test "Parse '2*3*10/4'" do
      let r = parse term "2*3*10/4"
      assertParser r 15.0 ""
