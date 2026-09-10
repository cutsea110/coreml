{-# LANGUAGE DuplicateRecordFields #-}
module DFA where

import Control.Monad (foldM)
import Data.List (nub, (\\))

-- ユーティリティ
flatten :: Eq a => [[a]] -> [a]
flatten = nub . concat
hasAnyOf :: Eq a => [a] -> [a] -> Bool
fs `hasAnyOf` xs = any (`elem` fs) xs

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
               } deriving (Show, Eq)

type State = [Q]

type Delta' = [(State, [(S, State)])]

data DFA = DFA { q     :: [State]
               , s     :: [S]
               , delta :: Delta'
               , q0    :: State
               , f     :: [State]
               } deriving (Show, Eq)

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
epsilonCl d ps = flatten [epsilonTransition p|p  <- ps]
  where epsilonTransition p = go [p]
          where
            go qs
              | qs == qs' = qs
              | otherwise = go qs'
              where qs' = flatten $ qs:[q' | q <- qs, q' <- transitions d (q, epsilon)]

{-|

- 単純なケース

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

- 連接

>>> d = [(0, (epsilon, [1])), (1, ("a", [2])), (2, (epsilon, [3])), (3, ("b", [4])), (4, (epsilon, [5]))]
>>> delta' d (0, "ab")
[4,5]

- 連接 (r1r2)

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("b", [4])), (3, ("b", [5])), (4, ("a", [5]))]
>>> delta' d (0, "ab")
[5]
>>> delta' d (0, "ba")
[5]

- 選択 (r1|r2)

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("b", [4])), (3, (epsilon, [5])), (4, (epsilon, [5]))]
>>> delta' d (0, "a")
[3,5]
>>> delta' d (0, "b")
[4,5]

- 閉包 (r*)

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
delta' d (p, wa) = epsilonCl d $ flatten [qs | q <- delta' d (p, w), qs <- transitions d (q, a)]
  where w = init wa
        a = last wa:[]

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
transitions d (p, a) = [qs' | (p', (symbol, qs')) <- d, p' == p && symbol == a]

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

>>> (_, n12) = runFresh (appendNFA n1 n2 ["ab"]) (Env 4)
>>> lang n12
["ab"]
>>> [x ++ y | x <- lang n1, y <- lang n2] == lang n12
True

- 選択 (r1|r2) : L(Nr1|r2) = L(Nr1) `union` L(Nr2)

>>> (_, nChoice) = runFresh (alterNFA n1 n2 ["a","b"]) (Env 4)
>>> lang nChoice
["a","b"]
>>> (lang n1 ++ lang n2) == lang nChoice
True

- 閉包 (r1*) : L(Nr1*) = L(Nr1)* = {""} `union` L(Nr1) `union` L(Nr1)L(Nr1) `union` ...

>>> (_, nStar) = runFresh (closureNFA n1 ["", "a", "aa", "aaa"]) (Env 2)
>>> lang nStar
["","a","aa","aaa"]
-}
lang :: NFA -> [S]
lang (NFA _ ws d q0 fs)
  = [w | w <- ws, fs `hasAnyOf` delta' d (q0, w)]

{-|
何も読まずにε（空文字列）だけを受理するNFA。r? のような「省略可能」を
alterNFA と組み合わせて表現するときの、もう片方の選択肢として使う。

>>> (_, n) = runFresh (emptyNFA ["", "a"]) defEnv
>>> lang n
[""]
-}
emptyNFA :: [S] -> Fresh NFA
emptyNFA ws = do
  p <- fresh
  pure (NFA [p] ws [] p [p])

{-|
何も受理しない（空集合Φの）NFA。開始状態と受理状態を別々にし、間に
一切遷移を作らないことで表現する。choiceNFA（N項の選択）の空リストの
場合に、選択の単位元（Φ ∪ L = L）として使う。

>>> (_, n) = runFresh (noneNFA ["", "a"]) defEnv
>>> lang n
[]
-}
noneNFA :: [S] -> Fresh NFA
noneNFA ws = do
  p <- fresh
  q <- fresh
  pure (NFA [p,q] ws [] p [q])

{-|
Nr1 = (Q1,Σ,δ1,p1,[q1]), Nr2 = (Q2,Σ,δ2,p2,[q2]) から
Nr1r2 = (Q1++Q2++{p,q}, Σ, δ1++δ2++{(p,[(ε,[p1])]),(q1,[(ε,[p2])]),(q2,[(ε,[q])])}, p, [q])
を作る。新規状態 p, q は Env から採番する。2項の連接なので `(++)` に倣って
appendNFA という名前にしている（N個まとめて連接するのは concatNFA、`concat` 相当）。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> n2 = NFA [2,3] ["b"] [(2,("b",[3]))] 2 [3]
>>> runFresh (appendNFA n1 n2 ["ab"]) (Env 4)
(Env {getEnv = 6},NFA {q = [0,1,2,3,4,5], s = ["ab"], delta = [(0,("a",[1])),(2,("b",[3])),(4,("",[0])),(1,("",[2])),(3,("",[5]))], q0 = 4, f = [5]})
-}
appendNFA :: NFA -> NFA -> [S] -> Fresh NFA
appendNFA (NFA qs1 _ d1 p1 [fq1]) (NFA qs2 _ d2 p2 [fq2]) ws = do
  p  <- fresh
  qf <- fresh
  pure NFA { q     = qs1 ++ qs2 ++ [p, qf]
           , s     = ws
           , delta = d1 ++ d2 ++ [ (p,   (epsilon, [p1]))
                                 , (fq1, (epsilon, [p2]))
                                 , (fq2, (epsilon, [qf]))
                                 ]
           , q0    = p
           , f     = [qf]
           }
appendNFA _ _ _ = error "appendNFA: f must be a singleton list"

{-|
NFA のリストを1つに連接する。`concat = foldr (++) []` に倣い、
appendNFA を畳み込んで作る。空リストは連接の単位元である emptyNFA（εだけを受理）になる。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> n2 = NFA [2,3] ["b"] [(2,("b",[3]))] 2 [3]
>>> n3 = NFA [4,5] ["c"] [(4,("c",[5]))] 4 [5]
>>> (_, n123) = runFresh (concatNFA [n1,n2,n3] ["abc"]) (Env 6)
>>> lang n123
["abc"]
-}
concatNFA :: [NFA] -> [S] -> Fresh NFA
concatNFA []       ws = emptyNFA ws
concatNFA (n:nfas) ws = foldM (\acc nfa -> appendNFA acc nfa ws) n nfas

{-|
Nr1 = (Q1,Σ,δ1,p1,[q1]), Nr2 = (Q2,Σ,δ2,p2,[q2]) から
Nr1|r2 = (Q1++Q2++{p,q}, Σ, δ1++δ2++{(p,[(ε,[p1,p2])]),(q1,[(ε,[q])]),(q2,[(ε,[q])])}, p, [q])
を作る。新規状態 p, q は Env から採番する。2項の選択なので alterNFA という名前にしている
（N個まとめて選択するのは choiceNFA）。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> n2 = NFA [2,3] ["b"] [(2,("b",[3]))] 2 [3]
>>> runFresh (alterNFA n1 n2 ["a","b"]) (Env 4)
(Env {getEnv = 6},NFA {q = [0,1,2,3,4,5], s = ["a","b"], delta = [(0,("a",[1])),(2,("b",[3])),(4,("",[0,2])),(1,("",[5])),(3,("",[5]))], q0 = 4, f = [5]})
-}
alterNFA :: NFA -> NFA -> [S] -> Fresh NFA
alterNFA (NFA qs1 _ d1 p1 [fq1]) (NFA qs2 _ d2 p2 [fq2]) ws = do
  p  <- fresh
  qf <- fresh
  pure NFA { q     = qs1 ++ qs2 ++ [p, qf]
           , s     = ws
           , delta = d1 ++ d2 ++ [ (p,   (epsilon, [p1,p2]))
                                 , (fq1, (epsilon, [qf]))
                                 , (fq2, (epsilon, [qf]))
                                 ]
           , q0    = p
           , f     = [qf]
           }
alterNFA _ _ _ = error "alterNFA: f must be a singleton list"

{-|
NFA のリストを1つの選択にまとめる。空リストは選択の単位元である
「何も受理しないNFA」（noneNFA）になる。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> n2 = NFA [2,3] ["b"] [(2,("b",[3]))] 2 [3]
>>> n3 = NFA [4,5] ["c"] [(4,("c",[5]))] 4 [5]
>>> (_, nChoice) = runFresh (choiceNFA [n1,n2,n3] ["a","b","c"]) (Env 6)
>>> lang nChoice
["a","b","c"]
-}
choiceNFA :: [NFA] -> [S] -> Fresh NFA
choiceNFA []       ws = noneNFA ws
choiceNFA (n:nfas) ws = foldM (\acc nfa -> alterNFA acc nfa ws) n nfas

{-|
Nr1 = (Q1,Σ,δ1,p1,[q1]) から
Nr1* = (Q1++{p,q}, Σ, δ1++{(p,[(ε,[p1,q])]),(q1,[(ε,[p1,q])])}, p, [q])
を作る。新規状態 p, q は Env から採番する。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> runFresh (closureNFA n1 ["", "a", "aa"]) (Env 2)
(Env {getEnv = 4},NFA {q = [0,1,2,3], s = ["","a","aa"], delta = [(0,("a",[1])),(2,("",[0,3])),(1,("",[0,3]))], q0 = 2, f = [3]})
-}
closureNFA :: NFA -> [S] -> Fresh NFA
closureNFA (NFA qs1 _ d1 p1 [fq1]) ws = do
  p  <- fresh
  qf <- fresh
  pure NFA { q     = qs1 ++ [p, qf]
           , s     = ws
           , delta = d1 ++ [ (p,   (epsilon, [p1,qf]))
                           , (fq1, (epsilon, [p1,qf]))
                           ]
           , q0    = p
           , f     = [qf]
           }
closureNFA _ _ = error "closureNFA: f must be a singleton list"

deltaDFA :: Delta -> (State, S) -> State
deltaDFA d (ps, a) = flatten [delta' d (p, a) | p <- ps]

addS :: NFA -> (State, S) -> ([State], [State], [(S, State)]) -> ([State], [(S, State)])
addS NFA { delta = d } (a, s) (q1, q2, omega) = (q1', [(s, a')] ++ omega)
  where a'  = deltaDFA d (a, s)
        q1' = if a' `elem` (a:q1 ++ q2) then q1 else a':q1

addQ :: NFA -> State -> ([State], [State], Delta') -> ([State], [State], Delta')
addQ nfa@NFA { s = ws } a (q1, q2, d) = (q1n, a:q2, [(a, omegan)] ++ d)
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
DFA {q = [[3,5],[],[4,5],[0,1,2]], s = ["a","b"], delta = [([3,5],[("b",[]),("a",[])]),([],[("b",[]),("a",[])]),([4,5],[("b",[]),("a",[])]),([0,1,2],[("b",[4,5]),("a",[3,5])])], q0 = [0,1,2], f = [[3,5],[4,5]]}

- q0 は必ず q に含まれる（起点にε遷移があるため、修正前は `q0 = [q0]` がこの不変条件を破っていた）

>>> DFA qs _ _ q0' _ = toDFA nfa
>>> q0' `elem` qs
True

連接 (abab) : εを含まない、鎖状のNFA。各NFA状態がそのまま1つのDFA状態になる

>>> d2 = [(0, ("a", [1])), (1, ("b", [2])), (2, ("a", [3])), (3, ("b", [4]))]
>>> nfa2 = NFA [0..4] ["a","b"] d2 0 [4]
>>> toDFA nfa2
DFA {q = [[4],[3],[2],[1],[],[0]], s = ["a","b"], delta = [([4],[("b",[]),("a",[])]),([3],[("b",[4]),("a",[])]),([2],[("b",[]),("a",[3])]),([1],[("b",[2]),("a",[])]),([],[("b",[]),("a",[])]),([0],[("b",[]),("a",[1])])], q0 = [0], f = [[4]]}

>>> DFA qs2 _ _ q0'2 _ = toDFA nfa2
>>> q0'2 `elem` qs2
True

閉包 (a*) : closureNFA が受理状態から開始状態へεで戻す辺を持つため、
NFA自体が循環パターンになっている。
その結果DFA側の遷移表にも状態 [1,0,3] が "a" を読んで自分自身に戻る自己ループが現れる。

>>> n1 = NFA [0,1] ["a"] [(0,("a",[1]))] 0 [1]
>>> (_, nStar) = runFresh (closureNFA n1 ["a"]) (Env 2)
>>> toDFA nStar
DFA {q = [[1,0,3],[2,0,3]], s = ["a"], delta = [([1,0,3],[("a",[1,0,3])]),([2,0,3],[("a",[1,0,3])])], q0 = [2,0,3], f = [[1,0,3],[2,0,3]]}

>>> DFA qs3 _ _ q0'3 _ = toDFA nStar
>>> q0'3 `elem` qs3
True
-}
toDFA :: NFA -> DFA
toDFA nfa@(NFA _ ws d q0 fs)
  = DFA { q     = qs'
        , s     = ws
        , delta = d'
        , q0    = a
        , f     = fs'
        }
  where a         = epsilonCl d [q0]
        (qs', d') = subsets nfa ([a], [], [])
        fs'       = [ a' | a' <- qs', fs `hasAnyOf` a' ]

type Block     = [State]
type Partition = [Block]

{-|
toDFA が作る DFA は部分集合構成法だけを行うので、言語として等価な状態でも
別々のDFA状態のままになっている（最小化されていない）。minimizeDFA はそれを
素朴な分割再帰法（Moore法）でまとめ、最小のDFAを作る。

やり方:

1. 最終状態と非最終状態の2ブロックに分ける（これ以上は絶対に混ざれない）。
2. 各ブロックについて、記号ごとの遷移先が属するブロックが状態同士で
   食い違っていたら、そのブロックを割る。
3. 分割が変化しなくなるまで2を繰り返す。
4. 安定したブロック1つを新しい1状態とみなしてDFAを組み直す
   （各ブロックの代表元として最小の State を採用する）。

>>> d = [(0, (epsilon, [1,2])), (1, ("a", [3])), (2, ("b", [4])), (3, (epsilon, [5])), (4, (epsilon, [5]))]
>>> nfa = NFA [0..5] ["a","b"] d 0 [5]
>>> dfa = toDFA nfa
>>> dfa
DFA {q = [[3,5],[],[4,5],[0,1,2]], s = ["a","b"], delta = [([3,5],[("b",[]),("a",[])]),([],[("b",[]),("a",[])]),([4,5],[("b",[]),("a",[])]),([0,1,2],[("b",[4,5]),("a",[3,5])])], q0 = [0,1,2], f = [[3,5],[4,5]]}
>>> minimizeDFA dfa
DFA {q = [[3,5],[],[0,1,2]], s = ["a","b"], delta = [([3,5],[("a",[]),("b",[])]),([],[("a",[]),("b",[])]),([0,1,2],[("a",[3,5]),("b",[3,5])])], q0 = [0,1,2], f = [[3,5]]}
-}
minimizeDFA :: DFA -> DFA
minimizeDFA (DFA qs ws d q0 fs)
  = DFA { q     = newQ
        , s     = ws
        , delta = newDelta
        , q0    = rep q0
        , f     = newF
        }
  where
    target :: State -> S -> State
    target st sym = maybe [] id $ do
      row <- lookup st d
      lookup sym row

    classOf :: Partition -> State -> Int
    classOf p st = head [ i | (i, blk) <- zip [0 :: Int ..] p, st `elem` blk ]

    sigOf :: Partition -> State -> [Int]
    sigOf p st = [ classOf p (target st sym) | sym <- ws ]

    splitBlock :: Partition -> Block -> [Block]
    splitBlock p blk = nub [ [ st' | (st', sg') <- tagged, sg' == sg ] | (_, sg) <- tagged ]
      where tagged = [ (st, sigOf p st) | st <- blk ]

    refine :: Partition -> Partition
    refine p = concatMap (splitBlock p) p

    stabilize :: Partition -> Partition
    stabilize p
      | length p' == length p = p
      | otherwise              = stabilize p'
      where p' = refine p

    finalP :: Partition
    finalP = stabilize (filter (not . null) [fs, qs \\ fs])

    rep :: State -> State
    rep st = minimum (finalP !! classOf finalP st)

    newQ     = map minimum finalP
    newF     = [ minimum blk | blk <- finalP, any (`elem` fs) blk ]
    newDelta = [ (minimum blk, [ (sym, rep (target (head blk) sym)) | sym <- ws ]) | blk <- finalP ]

{-|
DFA d に文字列 w を実際に食わせて受理するか判定する。1文字ずつ delta を辿り、
最後にいる状態が f に含まれるかを見るだけ。遷移が見つからない場合（trap状態への
遷移や、アルファベットに無い文字を読んだ場合）は空の State に落として拒否として扱う。


- [a-c] : charsets で作った文字集合例

>>> (_, nfa) = runFresh (charsets "abc") defEnv
>>> dfa = minimizeDFA (toDFA nfa)
>>> map (runDFA dfa) ["a","b","c","d","","ab","ac"]
[True,True,True,False,False,False,False]


- [a-c]* : charsets と closureNFA を do 記法でつなぐ例

>>> starBuild = do { nabc <- charsets "abc"; closureNFA nabc ["a","b","c"] }
>>> (_, nstar) = runFresh starBuild defEnv
>>> starDfa = minimizeDFA (toDFA nstar)
>>> map (runDFA starDfa) ["", "a", "abc", "aabbcc", "cba", "aaaa", "d", "abcd", "ab1"]
[True,True,True,True,True,True,False,False,False]


- abc : char と concatNFA（N項版）を do 記法でつなぐ、文字列リテラル例

>>> abcAlphabet = ["a","b","c"]
>>> abcBuild = do { na <- char 'a'; nb <- char 'b'; nc <- char 'c'; concatNFA [na,nb,nc] abcAlphabet }
>>> (e4, nabcSeq) = runFresh abcBuild defEnv
>>> abcDfa = minimizeDFA (toDFA nabcSeq)
>>> map (runDFA abcDfa) ["a","b","c","d","","ab","ac","abc"]
[False,False,False,False,False,False,False,True]


- (abc)* : 上の nabcSeq に closureNFA をかぶせるだけの例

>>> (_, nabcStar) = runFresh (closureNFA nabcSeq abcAlphabet) e4
>>> abcStarDfa = minimizeDFA (toDFA nabcStar)
>>> map (runDFA abcStarDfa) ["", "abc", "abcabc", "abcabcabc", "ab", "abca", "abcabx", "xabc"]
[True,True,True,True,False,False,False,False]


- (-?)[0-9]+ : emptyNFA で「-の省略」を、charsets を2回使って「最初の1桁」と「0回以上の繰り返し」を分けて組み立て、
  最後に concatNFA（N項版）で「省略可能な-」「最初の1桁」「0回以上の繰り返し」の3つを一度に連接する、まとめて1つの do 記法で書く例

>>> digits = ['0'..'9']
>>> digitAlphabet = [ [c] | c <- digits ]
>>> numAlphabet = "-" : digitAlphabet
>>> numBuild = do { nDash <- char '-'; nEps <- emptyNFA numAlphabet; nOptDash <- alterNFA nDash nEps numAlphabet; nDigits1 <- charsets digits; nDigits2 <- charsets digits; nDigitsStar <- closureNFA nDigits2 numAlphabet; concatNFA [nOptDash, nDigits1, nDigitsStar] numAlphabet }
>>> (_, nNum) = runFresh numBuild defEnv
>>> numDfa = minimizeDFA (toDFA nNum)
>>> map (runDFA numDfa) ["123", "-123", "0", "-0", "007", "", "-", "12a", "--12", "12-"]
[True,True,True,True,True,False,False,False,False,False]
-}
runDFA :: DFA -> String -> Bool
runDFA (DFA _ _ d q0 fs) w = foldl step q0 w `elem` fs
  where
    step st c = maybe [] id $ do
      row <- lookup st d
      lookup [c] row

newtype Env = Env { getEnv :: Int } deriving (Show, Eq)
defEnv :: Env
defEnv = Env 0
getNext :: Env -> (Int, Env)
getNext (Env n) = (n, Env (n + 1))

{-|
Env を持ち回る小さな状態モナド。`char`/`charsets`/`appendNFA`/... はどれも
「必要なだけ新しい状態番号を採番しながらNFAを組み立てる」計算なので、
Env を明示的な引数として毎回受け渡す代わりにこの型でラップし、
`do` 記法で書けるようにする。

>>> runFresh (pure 'x') defEnv
(Env {getEnv = 0},'x')
>>> runFresh (do { a <- fresh; b <- fresh; pure (a,b) }) defEnv
(Env {getEnv = 2},(0,1))
-}
newtype Fresh a = Fresh { runFresh :: Env -> (Env, a) }

instance Functor Fresh where
  fmap f (Fresh g) = Fresh $ \env -> let (env', a) = g env in (env', f a)

instance Applicative Fresh where
  pure a = Fresh $ \env -> (env, a)
  Fresh mf <*> Fresh ma = Fresh $ \env ->
    let (env1, f) = mf env
        (env2, a) = ma env1
    in (env2, f a)

instance Monad Fresh where
  Fresh ma >>= f = Fresh $ \env ->
    let (env1, a) = ma env
    in runFresh (f a) env1

fresh :: Fresh Q
fresh = Fresh (\env -> let (n, env') = getNext env in (env', n))

char :: Char -> Fresh NFA
char c = do
  s <- fresh
  e <- fresh
  pure (NFA [s,e] [sym] [(s,(sym,[e]))] s [e])
  where
    sym = [c]

{-|
[a-zA-Z] のような文字集合を1つのNFAにする。`alterNFA` を鎖状に繰り返し畳み込むと
文字数に比例した長さのε遷移の鎖ができてしまい、`epsilonCl` の不動点計算がその鎖を
1ホップずつしか進められないため文字数が増えると急激に遅くなる。
そこで新しい開始状態1つから各文字のNFAへε分岐、各文字のNFAの受理状態から
新しい受理状態1つへε収束、という1段のfan-out/fan-inで組み立てる。

>>> (_, NFA nq _ nd nq0 nf) = runFresh (charsets "abc") defEnv
>>> lang (NFA nq ["a","b","c","d","","ab","ac"] nd nq0 nf)
["a","b","c"]
-}
charsets :: [Char] -> Fresh NFA
charsets []  = error "charsets: empty charsets"
charsets cs = do
  nfas <- mapM char cs
  p  <- fresh
  qf <- fresh
  let qss    = [ qsI | NFA qsI _  _  _  _    <- nfas ]
      dss    = [ dI  | NFA _   _  dI _  _    <- nfas ]
      starts = [ p0  | NFA _   _  _  p0 _    <- nfas ]
      fins   = [ fq  | NFA _   _  _  _  [fq] <- nfas ]
  pure NFA { q     = concat qss ++ [p, qf]
           , s     = ws
           , delta = concat dss ++ (p, (epsilon, starts)) : [ (fq, (epsilon, [qf])) | fq <- fins ]
           , q0    = p
           , f     = [qf]
           }
  where
    ws = [ [c] | c <- cs ]
