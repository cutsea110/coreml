module Compiler where

import TM (Q(..), Delta, A(..), S(..), D(..), Tape, showTape)
import Eval (eval)

data Env = Env { state :: Int
               }
         deriving (Show, Eq)

get :: Env -> TM.Q
get env = TM.S (state env)
next :: Env -> Env
next env = env { state = state env + 1 }
defEnv :: Env
defEnv = Env { state = 0 }

type Compiler = Env -> (Env, TM.Delta)

compose :: Compiler -> Compiler -> Compiler
(c1 `compose` c2) env0 = (env2, code1 ++ code2)
  where
    (env1, code1) = c1 env0
    (env2, code2) = c2 env1

-- | 条件分岐: ヘッドが1のときc1を、0か空白のときc2を実行する
branch :: Compiler -> Compiler -> Compiler
branch c1 c2 env0 = (env2, dispatch ++ code1 ++ code2)
  where
    -- 二つの分岐それぞれに、独立した開始状態を割り当てる。
    -- c1 の停止状態に c2 を開始してはいけない。c1 はその状態に
    -- 到達することで停止するためである。
    (env1, code1) = c1 (next env0)
    (env2, code2) = c2 (next env1)

    s0 = get env0
    s1 = get (next env0)
    s2 = get (next env1)

    dispatch = [ ((s0, TM.I), (s1, TM.Nop))
               , ((s0, TM.O), (s2, TM.Nop))
               , ((s0, TM.B), (s2, TM.Nop))
               ]

-- | 右へ空白をスキップして1,0で停止
skipBlank :: Env -> (Env, TM.Delta)
skipBlank env0 = (env1, code)
  where
    code = [ ((s0, TM.B), (s0, TM.Move TM.R))
           , ((s0, TM.I), (s1, TM.Nop))
           , ((s0, TM.O), (s1, TM.Nop))
           ]
    s0   = get env0
    s1   = get env1
    env1 = next env0

-- | 右へ1,0の列をスキップして空白で停止
skipSeq :: Env -> (Env, TM.Delta)
skipSeq env0 = (env1, code)
  where
    code = [ ((s0, TM.I), (s0, TM.Move TM.R))
           , ((s0, TM.O), (s0, TM.Move TM.R))
           , ((s0, TM.B), (s1, TM.Nop))
           ]
    s0   = get env0
    s1   = get env1
    env1 = next env0

-- | 最下位桁にいる状態から1を加える
add1 :: Compiler
add1 env0 = (env2, code)
  where
    code = [ ((s0, TM.I), (s1, TM.Write TM.O))
           , ((s0, TM.O), (s2, TM.Write TM.I))
           , ((s0, TM.B), (s2, TM.Write TM.I))
           , ((s1, TM.I), (s0, TM.Move TM.L))
           , ((s1, TM.O), (s0, TM.Move TM.L))
           ]
    s0   = get env0
    s1   = get env1
    s2   = get env2
    env1 = next env0
    env2 = next env1

-- | 最下位桁にいる状態から1を減らす
sub1 :: Compiler
sub1 env0 = (env2, code)
  where
    code = [ ((s0, TM.I), (s2, TM.Write TM.O))
           , ((s0, TM.O), (s1, TM.Write TM.I))
           , ((s0, TM.B), (s2, TM.Write TM.B))
           , ((s1, TM.I), (s0, TM.Move TM.L))
           , ((s1, TM.O), (s0, TM.Move TM.L))
           ]
    s0   = get env0
    s1   = get env1
    s2   = get env2
    env1 = next env0
    env2 = next env1


-- | x y と2つの列が空白で区切られている状態で y の最下位桁にいる状態から、x+yを計算して x+y 0 の列を作る
plus :: Compiler
plus = undefined


test :: Compiler -> Tape -> IO ()
test comp ini = do
  let (_, p) = comp defEnv
      res    = eval (S 0, p) ini
  putStr $ merge (showTape ini) (showTape res)
  where
    merge :: [String] -> [String] -> String
    merge [lu, ll] [ru, rl] = unlines [lu ++ " --> " ++ ru, ll' ++ "     " ++ rl]
      where ll'  = ll ++ replicate (length lu - length ll) ' '
    merge _        _        = error "merge: invalid input"
