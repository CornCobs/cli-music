{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Parser where

import Text.Parsec
import qualified Data.Text as T
import Data.Ratio
import Control.Monad (void)

import Types

type MelodyParser = Parsec T.Text Octave

runMelodyParser :: T.Text -> Either ParseError [Melody]
runMelodyParser score = runParser (many1 parseMelody) 0 "" score

parseLen :: MelodyParser Rational
parseLen = choice [frac, toRational <$> number, return 1] >>= dotMultiplier
  where frac = (1 %) <$> (char '/' *> number)
        dotMultiplier len = do 
          numDots <- length <$> many (char '.')
          return $ iterate (* (3 % 2)) len !! numDots

parseNote :: MelodyParser Note
parseNote = do
  octave <- getState
  pitch <- (read . (:[])) <$> oneOf ['C','D','E','F','G','A','B']
  let baseNote = Note pitch octave
  modifier baseNote
  where sharps = length <$> many1 (char '+')
        flats = (negate . length) <$> many1 (char '-')
        getModifiers = choice [sharps, flats, return 0]
        modifier note = do
          mods <- getModifiers
          let result | mods == 0 = note
                     | mods >  0 = iterate sharp note !! mods
                     | mods <  0 = iterate flat  note !! (abs mods)
          return result

parseOctaveChange :: MelodyParser ()
parseOctaveChange = choice [char '>' *> modifyState (+1), char '<' *> modifyState (subtract 1)]

parseChord :: MelodyParser Chord
parseChord = 
  let parseSingle = (:[]) <$> parseNote
      parseWithinParen = sepEndBy parseOctaveChange spaces
                         *> parseNote
                         <* spaces
                         <* sepEndBy parseOctaveChange spaces
      parseParen = localState $ between (char '(') (char ')') $ many parseWithinParen
      parseRest = char '_' *> return []
  in label (do notes <- parseSingle <|> parseParen <|> parseRest
               len <- parseLen
               return $ Chord notes len)
           "parseChord"

parseMelody :: MelodyParser Melody
parseMelody = do
  separators *> string "speed:" *> spaces
  speed <- fromInteger <$> number
  separators *> string "octave:" *> spaces
  octave <- fromInteger <$> number
  putState (Octave octave)
  separators
  tune <- between (char '{' *> spaces) (char '}') $ sepEndBy parseWithinBrace separators
  return $ Melody speed tune
  where parseWithinBrace = sepEndBy parseOctaveChange spaces
                         *> parseChord
                         <* spaces
                         <* sepEndBy parseOctaveChange spaces



-- Utility functions

ignoreComment :: MelodyParser ()
ignoreComment = spaces *> char '#' *> manyTill anyChar eol *> spaces

separators :: MelodyParser ()
separators = spaces *> optional (many ignoreComment)

number :: MelodyParser Integer
number = read <$> many1 digit

eol :: MelodyParser ()
eol = void (newline <|> crlf)

localState :: MelodyParser a -> MelodyParser a
localState parser = do
  octave <- getState
  result <- parser
  putState octave
  return result