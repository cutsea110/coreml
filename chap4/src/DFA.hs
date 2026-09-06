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

epsilonCl :: Delta -> [Q] -> [Q]
epsilonCl d ps = flatten [ qs
                         | p <- ps
                         , qs <- [ qs'
                                 | (p', (symbol, qs')) <- d
                                 , p' == p && symbol == epsilon
                                 ]
                         ]
  where flatten = nub . concat
