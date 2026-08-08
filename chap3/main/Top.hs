module Top
  ( top
  , topWithStdIn
  ) where

import System.IO (Handle, IOMode(ReadMode), hClose, openFile, stdin)
import qualified ReadString
import Control.Exception (catch)

readAndPrintLoop :: Handle -> IO ()
readAndPrintLoop inStream = do
  _ <- ReadString.skipSpaces inStream
  s <- ReadString.readString inStream
  putStrLn s
  readAndPrintLoop inStream

top :: FilePath -> IO ()
top file = do
  inStream <- openFile file ReadMode
  readAndPrintLoop inStream
  hClose inStream
  `catch`
    \ReadString.EOF -> return ()

topWithStdIn :: IO ()
topWithStdIn = do
  let inStream = stdin
  readAndPrintLoop inStream
  hClose inStream
  `catch`
    \ReadString.EOF -> return ()
