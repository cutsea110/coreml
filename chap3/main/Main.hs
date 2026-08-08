module Main where

import System.Environment (getArgs)
import Top (top, topWithStdIn)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (h:_) -> Top.top h
    []    -> Top.topWithStdIn
