module Top
  ( top
  , topWithStdIn
  ) where

import System.IO (IOMode(ReadMode), hClose, openFile, stdin)
import qualified Lexer
import qualified Token

readAndPrintLoop :: IO Token.Token -> IO ()
readAndPrintLoop nextToken = do
  tok <- nextToken
  case tok of
    Token.EOF -> return ()
    _ -> do
      putStrLn (Token.toString tok)
      readAndPrintLoop nextToken

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
