module Main (main) where

import System.Environment (getArgs)
import Top (top, topWithStdIn)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> topWithStdIn
    f:_ -> top f
