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

nextHead :: Env -> (Env, TM.Delta)
nextHead env0 = (env1, code)
  where
    code = [ ((s0, TM.B), (s0, TM.Move TM.R))
           , ((s0, TM.I), (s1, TM.Nop))
           , ((s0, TM.O), (s1, TM.Nop))
           ]
    s0   = get env0
    s1   = get env1
    env1 = next env0

test :: IO ()
test = do
  let (_, p) = nextHead defEnv
      init   = ([], B, [B,B,B,B,O,I])
      res    = eval (S 0, p) init
  putStr $ merge (showTape init) (showTape res)
  where
    merge :: [String] -> [String] -> String
    merge [lu, ll] [ru, rl] = unlines [lu ++ " --> " ++ ru, ll' ++ "     " ++ rl]
      where lenl = length lu
            lenr = length ru
            ll'  = ll ++ replicate (length lu - length ll) ' '

