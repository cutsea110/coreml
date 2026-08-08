module TM where

data D = R -- ^ move right
       | L -- ^ move left
       deriving (Show, Eq)

data S = B -- ^ blank
       | I -- ^ 1
       | O -- ^ 0
       deriving (Show, Eq)

data Q = M -- ^ move
       | H -- ^ halt
       deriving (Show, Eq)

type Delta = [((Q, S), (Q, S, D))]

type Program = (Q, Delta)

type Tape = ([S], S, [S])

p :: Program
p = (M, [ ((M, I), (M, O, L))
        , ((M, O), (H, I, L))
        , ((M, B), (H, I, L))
        ])
