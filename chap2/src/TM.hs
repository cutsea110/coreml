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

addOne :: Program
addOne = (S 0, [ ((S 0, I), (S 1, Write O))
               , ((S 0, O), (S 2, Write I))
               , ((S 0, B), (S 2, Write I))
               , ((S 1, I), (S 0, Move L))
               , ((S 1, O), (S 0, Move L))
               ])

subOne :: Program
subOne = (S 0, [ ((S 0, I), (S 2, Write O))
               , ((S 0, O), (S 1, Write I))
               , ((S 0, B), (S 2, Write B))
               , ((S 1, I), (S 0, Move L))
               , ((S 1, O), (S 0, Move L))
               ])
