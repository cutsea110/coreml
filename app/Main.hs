module Main (main) where

import Eval (eval)
import TM   (Tape, S(..), p)

t :: Tape
t = ([I, I, I], I, [])

r :: Tape
r = eval p t

main :: IO ()
main = print (t, r)
