module Main (main) where

import Data.List (nub)

import DFA

-- メールアドレス的なもののマッチをする
--
-- ざっくりしたパターン: [ローカル部]+ @ [ドメイン部]+ . [TLD部]+
--   ローカル部: 英数字(小文字) . _ % + -
--   ドメイン部: 英数字(小文字) . -
--   TLD部     : 英字(小文字)
-- Σを小さく保つため大文字は対象外にしている(状態数・遷移数がΣのサイズに
-- 直接比例して膨らむため; 詳しくは toDFA/subsets のコメント参照)。
buildEmailNFA :: Fresh NFA
buildEmailNFA = do
  local  <- charset localChars  >>= plusNFA
  at     <- char '@'
  domain <- charset domainChars >>= plusNFA
  dot    <- char '.'
  tld    <- charset tldChars    >>= plusNFA
  concatNFA [local, at, domain, dot, tld]
  where
    localChars  = ['a'..'z'] ++ ['0'..'9'] ++ "._%+-"
    domainChars = ['a'..'z'] ++ ['0'..'9'] ++ ".-"
    tldChars    = ['a'..'z']

emailAlphabet :: [S]
emailAlphabet = nub [ [c] | c <- chars ]
  where
    chars = ['a'..'z'] ++ ['0'..'9'] ++ "._%+-@"

emailDFA :: DFA
emailDFA = toDFA' (snd (runFresh buildEmailNFA (newEnv emailAlphabet)))

testCases :: [String]
testCases =
  [ "foo@example.com"
  , "john.doe@sub.example.co.jp"
  , "a@b.cd"
  , "foo.bar+baz@qux-quux.org"
  , "invalid.email"
  , "@example.com"
  , "foo@.com"
  , "foo@bar."
  , "foo@bar"
  , ""
  ]

main :: IO ()
main = mapM_ report testCases
  where
    report w = putStrLn (show (runDFA emailDFA w) ++ "\t" ++ w)
