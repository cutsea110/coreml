module Top
  ( top
  , topWithStdIn
  ) where

import System.IO (IOMode(ReadMode), hClose, openFile, stdin)
import Lexer (makeLexer)
import Token (Token(..))

readAndPrintLoop :: IO Token.Token -> IO ()
readAndPrintLoop lexer = do
  tok <- lexer
  case tok of
    Token.EOF -> return ()
    _ -> do
      print tok
      readAndPrintLoop lexer

top :: FilePath -> IO ()
top file = do
  inStream <- openFile file ReadMode
  lexer <- Lexer.makeLexer inStream
  readAndPrintLoop lexer
  hClose inStream

topWithStdIn :: IO ()
topWithStdIn = do
  let inStream = stdin
  lexer <- Lexer.makeLexer inStream
  readAndPrintLoop lexer
  hClose inStream
