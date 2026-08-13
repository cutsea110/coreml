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

-- | コンパイラの合成: c1の停止状態にc2を接続する
compose :: Compiler -> Compiler -> Compiler
(c1 `compose` c2) env0 = (env2, code1 ++ code2)
  where
    (env1, code1) = c1 env0
    (env2, code2) = c2 env1

-- | 条件分岐: ヘッドがcondのときc1を、それ以外のときc2を実行する
branch :: S -> Compiler -> Compiler -> Compiler
branch cond c1 c2 branchInitialEnv =
  (joinEnv, dispatch ++ c1Code ++ c2Code ++ join)
  where
    -- branchInitialState で条件を調べ、c1 または c2 の開始状態へ進む。
    -- 両方の終了状態は joinState に集め、branch の終了状態とする。
    branchInitialState = get branchInitialEnv

    -- c1 に渡す開始状態と、c1 が返す終了状態。
    c1InitialEnv = next branchInitialEnv
    (c1FinalEnv, c1Code) = c1 c1InitialEnv
    c1InitialState = get c1InitialEnv
    c1FinalState = get c1FinalEnv

    -- c1 の終了状態とは別の状態を、c2 の開始状態として確保する。
    c2InitialEnv = next c1FinalEnv
    (c2FinalEnv, c2Code) = c2 c2InitialEnv
    c2InitialState = get c2InitialEnv
    c2FinalState = get c2FinalEnv

    -- 二つの分岐を合流させる停止状態。後続の compose はここから始まる。
    joinEnv = next c2FinalEnv
    joinState = get joinEnv

    dispatch = [ ((branchInitialState, symbol),
                  (if symbol == cond then c1InitialState else c2InitialState, TM.Nop))
               | symbol <- [TM.I, TM.O, TM.B]
               ]

    -- c1 と c2 の停止状態を共通の合流状態へつなぐ。branch はこの
    -- 合流状態を返すため、compose で後続のコンパイラをどちらの分岐の
    -- 後にも接続できる。
    join = [ ((finalState, symbol), (joinState, TM.Nop))
           | finalState <- [c1FinalState, c2FinalState]
           , symbol <- [TM.I, TM.O, TM.B]
           ]

-- | ループ: ヘッドがcondのときcを実行し、それ以外のとき停止する
while :: S -> Compiler -> Compiler
while cond body whileInitialEnv =
  (whileFinalEnv, dispatch ++ bodyCode ++ loop)
  where
    -- whileInitialState で条件を調べ、条件を満たすと本体を実行する。
    whileInitialState = get whileInitialEnv

    -- 本体に渡す開始状態と、本体が返す終了状態。
    bodyInitialEnv = next whileInitialEnv
    (bodyFinalEnv, bodyCode) = body bodyInitialEnv
    bodyInitialState = get bodyInitialEnv
    bodyFinalState = get bodyFinalEnv

    -- 条件が偽のときに停止する状態。後続の compose はここから始まる。
    whileFinalEnv = next bodyFinalEnv
    whileFinalState = get whileFinalEnv

    dispatch = [ ((whileInitialState, symbol),
                  (if symbol == cond then bodyInitialState else whileFinalState, TM.Nop))
               | symbol <- [TM.I, TM.O, TM.B]
               ]

    -- 本体の終了状態から、条件を再び調べる状態へ戻る。
    loop = [ ((bodyFinalState, symbol), (whileInitialState, TM.Nop))
           | symbol <- [TM.I, TM.O, TM.B]
           ]

skip1s :: Compiler
skip1s env0 = while TM.I p env0
  where
    -- while がヘッドの値を確認してから呼び出すので、ここでは右へ
    -- 一文字移動するだけでよい。
    p e = (next e, [((s0, TM.I), (s1, TM.Move TM.R))])
      where
        s0 = get e
        s1 = get (next e)

skip0s :: Compiler
skip0s env0 = while TM.O p env0
  where
    p e = (next e, [((s0, TM.O), (s1, TM.Move TM.R))])
      where
        s0 = get e
        s1 = get (next e)


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
