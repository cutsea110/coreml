module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ "-isrc"
  , "lex/Lexer.hs"
  , "lex/Token.hs"
  , "lex/Parser.hs"
  , "main/Top.hs"
  ]
