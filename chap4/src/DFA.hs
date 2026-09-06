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

data DFA = DFA { q     :: [State]
               , s     :: [S]
               , delta :: Delta'
               , q0    :: State
               , f     :: [State]
               }
         deriving (Show, Eq)

{-|
>>> d = [(0, (epsilon, [1])), (1, ("a", [1])), (1, ("b", [2])), (2, ("a", [2]))]
>>> epsilonCl d [0]
[0,1]
>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, (epsilon, [4]))]
>>> epsilonCl d [0]
[0,1,2,4]
>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("b", [4])), (3, (epsilon, [5])), (4, (epsilon, [5]))]
>>> epsilonCl d [0]
[0,1,2]
>>> epsilonCl d [1]
[1]
>>> epsilonCl d [2]
[2]
>>> epsilonCl d [3]
[3,5]
>>> epsilonCl d [4]
[4,5]
-}
epsilonCl :: Delta -> State -> State
epsilonCl d ps
  = flatten [ epsilonTransition p
            | p <- ps
            ]
  where flatten = nub . concat
        epsilonTransition p = go [p]
          where
            go qs
              | qs == qs' = qs
              | otherwise = go qs'
              where qs' = nub (qs ++ flatten [ q' | q <- qs, q' <- transitions d (q, epsilon) ])

{-|

単純なケース

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("b", [4]))]
>>> delta' d (0, "a")
[3]
>>> delta' d (0, "b")
[4]

>>> d = [(0, ("a", [1])), (1, ("b", [2])), (2, ("a", [3])), (3, ("b", [4]))]
>>> delta' d (0, "a")
[1]
>>> delta' d (0, "ab")
[2]
>>> delta' d (0, "aba")
[3]
>>> delta' d (0, "abab")
[4]

連接

>>> d = [(0, (epsilon, [1])), (1, ("a", [2])), (2, (epsilon, [3])), (3, ("b", [4])), (4, (epsilon, [5]))]
>>> delta' d (0, "ab")
[4,5]

連接 (r1r2)

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("b", [4])), (3, ("b", [5])), (4, ("a", [5]))]
>>> delta' d (0, "ab")
[5]
>>> delta' d (0, "ba")
[5]

選択 (r1|r2)

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("b", [4])), (3, (epsilon, [5])), (4, (epsilon, [5]))]
>>> delta' d (0, "a")
[3,5]
>>> delta' d (0, "b")
[4,5]

閉包 (r*)

>>> d = [(0, (epsilon, [1,3])), (1, ("a", [2])), (2, (epsilon, [3])), (2, (epsilon, [1]))]
>>> delta' d (0, "a")
[2,3,1]
>>> delta' d (0, "aa")
[2,3,1]
>>> delta' d (0, "aaa")
[2,3,1]
>>> delta' d (0, "aaaaaaaaaa")
[2,3,1]
-}
delta' :: Delta -> (Q, S) -> State
delta' d (p, "") = epsilonCl d [p]
delta' d (p, wa) = epsilonCl d $ flatten [ qs
                                         | q <- delta' d (p, w)
                                         , qs <- transitions d (q, a)
                                         ]
  where w = init wa
        a = last wa:[]
        flatten = nub . concat

{-|
>>> d = [(0, ("a", [1])), (1, ("b", [2])), (2, ("c", [3])), (3, ("d", [4]))]
>>> transitions d (0, "a")
[[1]]
>>> transitions d (1, "b")
[[2]]

>>> d = [(0, ("a", [1,2])), (1, ("b", [3])), (1, ("c", [4]))]
>>> transitions d (0, "a")
[[1,2]]
>>> transitions d (1, "b")
[[3]]
>>> transitions d (1, "c")
[[4]]
>>> transitions d (3, "a")
[]
>>> transitions d (4, "b")
[]
>>> transitions d (3, epsilon)
[]
>>> transitions d (4, epsilon)
[]
-}
transitions :: Delta -> (Q, S) -> [State]
transitions d (p, a)
  = [ qs'
    | (p', (symbol, qs')) <- d
    , p' == p && symbol == a
    ]

