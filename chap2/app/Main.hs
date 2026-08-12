module Main (main) where

import Eval (eval)
import TM   (Tape, S(..), addOne, subOne)

t :: Tape
t = ([O, O, I], O, [])

r :: Tape
r = eval subOne t

main :: IO ()
main = print $ pair conv (t, r)
  where
    pair f (x, y) = (f x,f y)
    conv (ls, c, rs) = reverse ls ++ c:rs
