module Main where

import qualified Data.Text.IO as TIO
import System.Environment

import Sound
import Parser


main :: IO ()
main = do
  args <- getArgs
  case args of 
    [input] -> processFile input "output.raw"
    [input, output] -> processFile input output
    _ -> putStrLn "Usage: cli-music INPUT [OUTPUT]"

processFile :: FilePath -> FilePath -> IO ()
processFile inFile outFile = do 
  score <- TIO.readFile inFile
  case runMelodyParser score of
    Left err -> print err
    Right melody -> save outFile $ combineMelodiesToSound melody