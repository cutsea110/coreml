module Main (main) where

import Control.Monad (forM)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, (</>))
import Test.DocTest (doctest)

targetDir :: [FilePath]
targetDir = ["lex","main"]

main :: IO ()
main = do
  files <- concat <$> forM targetDir (\d -> do
    dir <- getDirectoryContents d
    return $ map (d </>) (filter (\f -> takeExtension f == ".hs") dir))
  doctest $ map ("-i" ++) targetDir ++ files
