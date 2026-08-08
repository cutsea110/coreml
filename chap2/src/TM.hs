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
       | PI -- ^ Init
       | PR -- ^ Scan to Right
       deriving (Show, Eq)

type Delta = [((Q, S), (Q, S, D))]

type Program = (Q, Delta)

type Tape = ([S], S, [S])

p :: Program
p = (PI, [ ((PI, B), (PR, B, R))
         , ((PR, O), (PR, O, R))
         , ((PR, I), (PR, I, R))
         , ((PR, B), (M, B, L))
         , ((M, I), (M, O, L))
         , ((M, O), (H, I, L))
         , ((M, B), (H, I, L))
         , ((H, I), (H, I, L))
         , ((H, O), (H, O, L))
         ])
