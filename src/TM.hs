module TM where

-- R: right
-- L: left
data D = R | L deriving (Show, Eq)
-- B: blank
-- I: 1
-- O: 0
data S = B | I | O deriving (Show, Eq)
-- M: move
-- H: halt
data Q = M | H deriving (Show, Eq)

type Delta = [((Q, S), (Q, S, D))]

type Program = (Q, Delta)

type Tape = ([S], S, [S])

p :: Program
p = (M, [ ((M, I), (M, O, L))
        , ((M, O), (M, I, L))
        , ((M, B), (H, I, L))
        ])
