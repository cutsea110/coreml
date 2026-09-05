module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, SeekMode(AbsoluteSeek), hFlush, hGetContents,
                   hPutStr, hSeek, openTempFile)

import Parser
import Token
import Prelude hiding (exp)

_runTest :: Parser a -> String -> [(a, Stream)]
_runTest p text = runParser p $ toStream text

-- | Build a 'Handle' whose contents are the given 'String', for use in
-- doctests that need a real file handle. The backing temp file is unlinked
-- immediately after creation; the handle stays valid until it is closed.
_stringHandle :: String -> IO Handle
_stringHandle s = do
  dir <- getTemporaryDirectory
  (path, h) <- openTempFile dir "lexer-doctest.txt"
  hPutStr h s
  hFlush h
  hSeek h AbsoluteSeek 0
  removeFile path
  return h

{-|
>>> _runTest alpha "abc"
[('a',[(1,'b'),(2,'c')])]
>>> _runTest alpha "XYZ"
[('X',[(1,'Y'),(2,'Z')])]
>>> _runTest alpha "123"
[]
>>> _runTest alpha "!@#"
[]
-}
alpha :: Parser Char
alpha = pSat isAlpha

{-|
>>> _runTest digit "123"
[('1',[(1,'2'),(2,'3')])]
>>> _runTest digit "abc"
[]
>>> _runTest digit "!@#"
[]
-}
digit :: Parser Char
digit = pSat isDigit

{-|
>>> _runTest alphaDigit "abc123"
[('a',[(1,'b'),(2,'c'),(3,'1'),(4,'2'),(5,'3')])]
>>> _runTest alphaDigit "XYZ"
[('X',[(1,'Y'),(2,'Z')])]
>>> _runTest alphaDigit "123"
[('1',[(1,'2'),(2,'3')])]
>>> _runTest alphaDigit "!@#"
[]
-}
alphaDigit :: Parser Char
alphaDigit = pSat isAlphaNum

{-|
>>> _runTest ident "abc123"
[("abc123",[])]
>>> _runTest ident "XYZ"
[("XYZ",[])]
>>> _runTest ident "123"
[]
>>> _runTest ident "!@#"
[]
>>> _runTest ident "_abc"
[]
-}
ident :: Parser String
ident = do
  c <- alpha
  cs <- pMunch (pSat isAlphaNum)
  return (c:cs)

{-|
>>> _runTest num "123"
[("123",[])]
>>> _runTest num "abc"
[]
>>> _runTest num "!@#"
[]
-}
num :: Parser String
num = pMunch1 digit

{-|
>>> _runTest frac ".123"
[(".123",[])]
>>> _runTest frac "123"
[]
>>> _runTest frac "!@#"
[]
-}
frac :: Parser String
frac = do
  _ <- pChar '.'
  ds <- num
  return ('.':ds)

{-|
>>> _runTest sign "-123"
[("-",[(1,'1'),(2,'2'),(3,'3')])]
>>> _runTest sign "123"
[("",[(0,'1'),(1,'2'),(2,'3')])]
-}
sign :: Parser String
sign = pLit "-" `pAltL` pEmpty ""

{-|
>>> _runTest exp "e-10"
[("e-10",[])]
>>> _runTest exp "E-5"
[("e-5",[])]
>>> _runTest exp "e7"
[("e7",[])]
>>> _runTest exp "E3"
[("e3",[])]
>>> _runTest exp "123"
[]
>>> _runTest exp "e+2"
[]
-}
exp :: Parser String
exp = do
  _ <- pSat (`elem` "eE")
  sg <- sign
  ds <- num
  return ('e':sg ++ ds)

{-|
>>> _runTest real "-123.45e-6"
[("-123.45e-6",[])]
>>> _runTest real "3.14"
[("3.14",[])]
>>> _runTest real "2e10"
[("2e10",[])]
-}
real :: Parser String
real = do
  sg <- sign
  n <- form1 `pAltL` form2
  return (sg ++ n)
  where
    form1 = do
      n <- num
      f <- frac `pAltL` pEmpty ""
      e <- exp
      return (n ++ f ++ e)
    form2 = do
      n <- num
      f <- frac
      e <- exp `pAltL` pEmpty ""
      return (n ++ f ++ e)

{-|
>>> _runTest ws "   "
[(" ",[(1,' '),(2,' ')])]
>>> _runTest ws "\t"
[("\t",[])]
>>> _runTest ws "\r\n"
[("\r\n",[])]
>>> _runTest ws "\n"
[("\n",[])]
>>> _runTest ws "\r"
[("\r",[])]
-}
ws :: Parser String
ws = pLit " " `pAltL` pLit "\t" `pAltL` pLit "\r\n" `pAltL` pLit "\n" `pAltL` pLit "\r"

{-|
>>> _runTest string "\"Hello, World!\""
[("Hello, World!",[])]
>>> _runTest string "\"This is a \\\"quoted\\\" string.\""
[("This is a \\\"quoted\\\" string.",[])]
-}
string :: Parser String
string = do
  _ <- pChar '"'
  cs <- pMunch strChar
  _ <- pChar '"'
  return (concat cs)
  where
    strChar = escaped `pAltL` normal
    escaped = pApply2 (\a b -> [a, b]) (pChar '\\') (pSat (const True))
    normal = (:[]) `pApply` pSat (/= '"')

{-|
>>> _runTest lexer ""
[(EOF,[])]
>>> _runTest lexer "   "
[(EOF,[])]
>>> _runTest lexer "\t \n\"Hello, World!\""
[(STRING "Hello, World!",[])]
>>> _runTest lexer "_123"
[(UNDERBAR,[(1,'1'),(2,'2'),(3,'3')])]
>>> _runTest lexer "\t\n6.02e23+3.13"
[(REAL 6.02e23,[(9,'+'),(10,'3'),(11,'.'),(12,'1'),(13,'3')])]
>>> _runTest lexer "abc"
[(ID "abc",[])]
>>> _runTest lexer "  abc"
[(ID "abc",[])]
>>> _runTest lexer "\tabc"
[(ID "abc",[])]
>>> _runTest lexer " @!#"
[(SPECIAL '@',[(2,'!'),(3,'#')])]
-}
lexer :: Parser Token
lexer = do
  _ <- pMunch ws
  f
  where
    f = (EOF <$ pEof) `pAltL`
        (STRING <$> string) `pAltL`
        (UNDERBAR <$ pChar '_') `pAltL`
        (REAL . read <$> real) `pAltL`
        (ID <$> ident) `pAltL`
        (SPECIAL <$> pSat (const True))

{-|
>>> h <- _stringHandle "abc 6.02e23 \"hi\""
>>> next <- makeLexer h
>>> next
ID "abc"
>>> next
REAL 6.02e23
>>> next
STRING "hi"
>>> next
EOF
>>> next
EOF
-}
-- | makeLexer takes a file handle and returns a stateful action that yields
-- the next Token on each call, threading the remaining Stream through an
-- IORef so the handle's contents are consumed one token at a time.
makeLexer :: Handle -> IO (IO Token)
makeLexer h = do
  ref <- newIORef . toStream =<< hGetContents h
  return $ do
    stream <- readIORef ref
    case runParser lexer stream of
      (tok, rest):_ -> do
        writeIORef ref rest
        return tok
      [] -> error "Lexer.makeLexer: lexer failed to produce a token"
