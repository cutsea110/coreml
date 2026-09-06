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

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("a", [4])), (3, (epsilon, [5])), (4, (epsilon, [5]))]
>>> transitions d (0, "a")
[[3],[4]]
-}
transitions :: Delta -> (Q, S) -> [State]
transitions d (p, a)
  = [ qs'
    | (p', (symbol, qs')) <- d
    , p' == p && symbol == a
    ]

{-|
- r = 空のとき L(Nr) = []

>>> n0 = NFA [0,1] [""] [] 0 [1]
>>> lang n0
[]

- r = a のとき L(Nr) = [a]

>>> na = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> lang na
["a"]

- 連接 (r1r2) : L(Nr1r2) = L(Nr1)L(Nr2)、ただし AB = [xy | x <- A, y <- B]

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> n2 = NFA [2,3] ["b"] [(2,("b",[3]))] 2 [3]
>>> lang n1
["a"]
>>> lang n2
["b"]

>>> n12 = concatNFA n1 n2 ["ab"]
>>> lang n12
["ab"]
>>> [x ++ y | x <- lang n1, y <- lang n2] == lang n12
True

- 選択 (r1|r2) : L(Nr1|r2) = L(Nr1) `union` L(Nr2)

>>> nChoice = choiceNFA n1 n2 ["a","b"]
>>> lang nChoice
["a","b"]
>>> (lang n1 ++ lang n2) == lang nChoice
True

- 閉包 (r1*) : L(Nr1*) = L(Nr1)* = {""} `union` L(Nr1) `union` L(Nr1)L(Nr1) `union` ...

>>> nStar = closureNFA n1 ["", "a", "aa", "aaa"]
>>> lang nStar
["","a","aa","aaa"]
-}
lang :: NFA -> [S]
lang (NFA { q = _qs, s = ws, delta = d, q0 = q0, f = fs })
  = [ w
    | w <- ws
    , any (`elem` fs) (delta' d (q0, w))
    ]

{-|
Nr1 = (Q1,Σ,δ1,p1,[q1]), Nr2 = (Q2,Σ,δ2,p2,[q2]) から
Nr1r2 = (Q1++Q2++{p,q}, Σ, δ1++δ2++{(p,[(ε,[p1])]),(q1,[(ε,[p2])]),(q2,[(ε,[q])])}, p, [q])
を作る。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> n2 = NFA [2,3] ["b"] [(2,("b",[3]))] 2 [3]
>>> concatNFA n1 n2 ["ab"]
NFA {q = [0,1,2,3,4,5], s = ["ab"], delta = [(0,("a",[1])),(2,("b",[3])),(4,("",[0])),(1,("",[2])),(3,("",[5]))], q0 = 4, f = [5]}
-}
concatNFA :: NFA -> NFA -> [S] -> NFA
concatNFA (NFA { q = qs1, delta = d1, q0 = p1, f = [fq1] })
          (NFA { q = qs2, delta = d2, q0 = p2, f = [fq2] })
          ws
  = NFA { q     = qs1 ++ qs2 ++ [p, qf]
        , s     = ws
        , delta = d1 ++ d2 ++ [ (p,   (epsilon, [p1]))
                               , (fq1, (epsilon, [p2]))
                               , (fq2, (epsilon, [qf]))
                               ]
        , q0    = p
        , f     = [qf]
        }
  where p  = maximum (qs1 ++ qs2) + 1
        qf = p + 1
concatNFA _ _ _ = error "concatNFA: f must be a singleton list"    

{-|
Nr1 = (Q1,Σ,δ1,p1,[q1]), Nr2 = (Q2,Σ,δ2,p2,[q2]) から
Nr1|r2 = (Q1++Q2++{p,q}, Σ, δ1++δ2++{(p,[(ε,[p1,p2])]),(q1,[(ε,[q])]),(q2,[(ε,[q])])}, p, [q])
を作る。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> n2 = NFA [2,3] ["b"] [(2,("b",[3]))] 2 [3]
>>> choiceNFA n1 n2 ["a","b"]
NFA {q = [0,1,2,3,4,5], s = ["a","b"], delta = [(0,("a",[1])),(2,("b",[3])),(4,("",[0,2])),(1,("",[5])),(3,("",[5]))], q0 = 4, f = [5]}
-}
choiceNFA :: NFA -> NFA -> [S] -> NFA
choiceNFA (NFA { q = qs1, delta = d1, q0 = p1, f = [fq1] })
          (NFA { q = qs2, delta = d2, q0 = p2, f = [fq2] })
          ws
  = NFA { q     = qs1 ++ qs2 ++ [p, qf]
        , s     = ws
        , delta = d1 ++ d2 ++ [ (p,   (epsilon, [p1,p2]))
                               , (fq1, (epsilon, [qf]))
                               , (fq2, (epsilon, [qf]))
                               ]
        , q0    = p
        , f     = [qf]
        }
  where p  = maximum (qs1 ++ qs2) + 1
        qf = p + 1
choiceNFA _ _ _ = error "choiceNFA: f must be a singleton list"

{-|
Nr1 = (Q1,Σ,δ1,p1,[q1]) から
Nr1* = (Q1++{p,q}, Σ, δ1++{(p,[(ε,[p1,q])]),(q1,[(ε,[p1,q])])}, p, [q])
を作る。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> closureNFA n1 ["", "a", "aa"]
NFA {q = [0,1,2,3], s = ["","a","aa"], delta = [(0,("a",[1])),(2,("",[0,3])),(1,("",[0,3]))], q0 = 2, f = [3]}
-}
closureNFA :: NFA -> [S] -> NFA
closureNFA (NFA { q = qs1, delta = d1, q0 = p1, f = [fq1] }) ws
  = NFA { q     = qs1 ++ [p, qf]
        , s     = ws
        , delta = d1 ++ [ (p,   (epsilon, [p1,qf]))
                         , (fq1, (epsilon, [p1,qf]))
                         ]
        , q0    = p
        , f     = [qf]
        }
  where p  = maximum qs1 + 1
        qf = p + 1
closureNFA _ _ = error "closureNFA: f must be a singleton list"

deltaDFA :: Delta -> (State, S) -> State
deltaDFA d (ps, a) = nub $ concat [ delta' d (p, a) | p <- ps]

addS :: NFA -> (State, S) -> ([State], [State], [(S, State)]) -> ([State], [(S, State)])
addS NFA { delta = d } (a, s) (q1, q2, omega) = (q1', [(s, a')] ++ omega)
  where a'  = deltaDFA d (a, s)
        q1' = if a' `elem` (a:q1 ++ q2) then q1 else a':q1

addQ :: NFA -> State -> ([State], [State], Delta') -> ([State], [State], Delta')
addQ nfa@(NFA { s = ws }) a (q1, q2, d) = (q1n, a:q2, [(a, omegan)] ++ d)
  where (q1n, omegan) = foldl phi (q1, []) ws
          where
            phi (q1i, omegai) si = addS nfa (a, si) (q1i, q2, omegai)

subsets :: NFA -> ([State], [State], Delta') -> ([State], Delta')
subsets _   ([],    qs2, d) = (qs2, d)
subsets nfa (a:qs1, qs2, d) = subsets nfa (addQ nfa a (qs1, qs2, d))

{-|

選択 (a|b) : εで分岐し、'a' または 'b' を1文字読んだ先で合流しないケース

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("b", [4])), (3, (epsilon, [5])), (4, (epsilon, [5]))]
>>> nfa = NFA [0..5] ["a","b"] d 0 [5]
>>> toDFA nfa
DFA {q = [[3,5],[],[4,5],[0,1,2]], s = ["a","b"], delta = [([3,5],[("b",[]),("a",[])]),([],[("b",[]),("a",[])]),([4,5],[("b",[]),("a",[])]),([0,1,2],[("b",[4,5]),("a",[3,5])])], q0 = [0], f = [[3,5],[4,5]]}

連接 (abab) : εを含まない、鎖状のNFA。各NFA状態がそのまま1つのDFA状態になる

>>> d2 = [(0, ("a", [1])), (1, ("b", [2])), (2, ("a", [3])), (3, ("b", [4]))]
>>> nfa2 = NFA [0..4] ["a","b"] d2 0 [4]
>>> toDFA nfa2
DFA {q = [[4],[3],[2],[1],[],[0]], s = ["a","b"], delta = [([4],[("b",[]),("a",[])]),([3],[("b",[4]),("a",[])]),([2],[("b",[]),("a",[3])]),([1],[("b",[2]),("a",[])]),([],[("b",[]),("a",[])]),([0],[("b",[]),("a",[1])])], q0 = [0], f = [[4]]}

閉包 (a*) : closureNFA が受理状態から開始状態へεで戻す辺を持つため、
NFA自体が循環パターンになっている。その結果DFA側の遷移表にも
状態 [1,0,3] が "a" を読んで自分自身に戻る自己ループが現れる。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> nStar = closureNFA n1 ["a"]
>>> toDFA nStar
DFA {q = [[1,0,3],[2,0,3]], s = ["a"], delta = [([1,0,3],[("a",[1,0,3])]),([2,0,3],[("a",[1,0,3])])], q0 = [2], f = [[1,0,3],[2,0,3]]}
-}
toDFA :: NFA -> DFA
toDFA (nfa@NFA { q = _qs, s = ws, delta = d, q0 = q0, f = fs })
  = DFA { q     = qs'
        , s     = ws
        , delta = d'
        , q0    = [q0]
        , f     = fs'
        }
  where a         = epsilonCl d [q0]
        (qs', d') = subsets nfa ([a], [], [])
        fs'       = [ a' | a' <- qs', any (`elem` fs) a' ]
