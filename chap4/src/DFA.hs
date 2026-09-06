{-# LANGUAGE DuplicateRecordFields #-}
module DFA where

import Data.List (nub)

type S = String

epsilon :: S
epsilon = ""

type Q = Int

type Delta = [(Q, (S, [Q]))]

data NFA = NFA { q     :: [Q]
               , s     :: [S]
               , delta :: Delta
               , q0    :: Q
               , f     :: [Q]
               }
         deriving (Show, Eq)

type State = [Q]

type Delta' = [(State, [(S, State)])]

data DFA = DFA { q      :: [State]
               , s      :: [S]
               , delta' :: Delta'
               , q0     :: State
               , f      :: [State]
               }
         deriving (Show, Eq)

{-|
>>> d = [(0, (epsilon, [1])), (1, ("a", [1])), (1, ("b", [2])), (2, ("a", [2]))]
>>> epsilonCl d [0]
[1]

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [1])), (1, (epsilon, [3])), (2, ("b", [2])), (3, ("a", [3]))]
>>> epsilonCl d [0]
[1,2]

>>> d = [(0, (epsilon, [1])), (0, (epsilon, [2])), (1, ("a", [1])), (2, ("b", [2])), (1, (epsilon, [0,3])), (2, (epsilon, [3])), (3, ("a", [3]))]
>>> epsilonCl d [0]
[1,2]
>>> epsilonCl d [1]
[0,3]
-}
epsilonCl :: Delta -> [Q] -> [Q]
epsilonCl d ps
  = flatten [ qs
            | p <- ps
            , qs <- transitions p
            ]
  where flatten = nub . concat
        -- epsilon transitions from a state p
        transitions p = [ qs'
                        | (p', (symbol, qs')) <- d
                        , p' == p && symbol == epsilon
                        ]
