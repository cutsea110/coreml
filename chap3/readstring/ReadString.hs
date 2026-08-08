module ReadString
  ( skipSpaces
  , readString
  , EOF(..)
  ) where

import Control.Exception (Exception, throwIO)
import Data.Char (isSpace)
import System.IO (Handle, hGetChar, hLookAhead, hIsEOF)

data EOF = EOF deriving (Show)
instance Exception EOF

skipSpaces :: Handle -> IO ()
skipSpaces inStream = do
  eof <- hIsEOF inStream
  if eof
    then throwIO EOF
    else do
      c <- hLookAhead inStream
      if isSpace c
        then do
          _ <- hGetChar inStream
          skipSpaces inStream
        else return ()

readString :: Handle -> IO String
readString inStream = readRest ""
  where
    readRest s = do
      c <- hLookAhead inStream
      if isSpace c
        then return s
        else do
        _ <- hGetChar inStream
        readRest (s ++ [c])
