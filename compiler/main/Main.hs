module Main (main) where

import System.Environment (getArgs)
import Top (top, topWithStdIn)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> Top.topWithStdIn
    f:_ -> Top.top f
