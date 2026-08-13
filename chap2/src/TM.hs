module TM where

data D = R -- ^ move right
       | L -- ^ move left
       deriving (Show, Eq)

data S = B -- ^ blank
       | I -- ^ 1
       | O -- ^ 0
       deriving (Show, Eq)

charS :: S -> Char
charS B = '_'
charS I = '1'
charS O = '0'

data A = Move D
       | Write S
       | Nop
       deriving (Show, Eq)

data Q = S Int
       deriving (Show, Eq)

type Delta = [((Q, S), (Q, A))]

type Program = (Q, Delta)

type Tape = ([S], S, [S])
conv :: Tape -> [S]
conv (ls, h, rs) = reverse ls ++ h:rs
showTape :: Tape -> [String]
showTape (ls, h, rs) = [map charS tape, replicate idx ' ' ++ "^"]
  where
    idx = length ls
    tape = reverse ls ++ [h] ++ rs
