{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Ratio

data Pitch = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B deriving (Eq, Show, Read, Ord, Enum)
data Note = Note Pitch Octave deriving (Eq, Ord, Show)

sharp :: Note -> Note
sharp (Note B o) = Note C (o + 1)
sharp (Note x o) = Note (succ x) o

flat :: Note -> Note
flat (Note C o) = Note B (o - 1)
flat (Note x o) = Note (pred x) o

newtype Octave = Octave Int deriving (Eq, Ord, Show, Num)

-- |The Chord type represents a note, or group of notes, played at the same time with same duration
-- An empty list denotes a rest note.
data Chord = Chord 
  { chordNotes :: [Note]
  , len :: Rational
  } 
  deriving (Show)

-- |The Melody type represents a full tune
-- Note that speed is in crotchets per minute 
data Melody = Melody 
  { speed :: Int
  , tune :: [Chord]
  }
  deriving (Show)