module TM where

data D = R -- ^ move right
       | L -- ^ move left
       deriving (Show, Eq)

data S = B -- ^ blank
       | I -- ^ 1
       | O -- ^ 0
       deriving (Show, Eq)

data A = Move D
       | Write S
       deriving (Show, Eq)

data Q = M -- ^ move
       | W -- ^ write
       | H -- ^ halt
       deriving (Show, Eq)

type Delta = [((Q, S), (Q, A))]

type Program = (Q, Delta)

type Tape = ([S], S, [S])

p :: Program
p = (W, [ ((W, I), (M, Write O))
        , ((W, O), (H, Write I))
        , ((W, B), (H, Write I))
        , ((M, I), (W, Move L))
        , ((M, O), (W, Move L))
        ])
