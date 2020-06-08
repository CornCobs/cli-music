module Main where

import qualified Data.Text.IO as TIO

import Sound
import Parser


main :: IO ()
main = do
  score <- TIO.readFile "holyholyholy.txt"
  case runMelodyParser score of 
    Left err -> print err
    Right melody -> save $ combineMelodiesToSound melody
