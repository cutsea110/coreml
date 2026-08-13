module Main (main) where

import Compiler (defEnv, add1, get)
import Eval (eval)
import TM   (Tape, conv, S(..))

t :: Tape
t = ([I, I, I], I, [])

r :: Tape
r = eval (s0, p) t
  where (_, p) = add1 defEnv
        s0 = get defEnv

main :: IO ()
main = print $ pair conv (t, r)
  where
    pair f (x, y) = (f x,f y)
