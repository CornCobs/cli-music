module Sound where 

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B

import Data.Foldable
import Data.List (uncons)

import Types

-- frequency is applied over a wave to modulate pitch
type Freq = Float -> Float

setFreq :: Float -> Freq
setFreq hertz = (* ((hertz * 2 * pi) / 48000))

-- |Frequencies of each pitch class at octave 4
c, cs, d, ds, e, f, fs, g, gs, a, as, b :: Freq
c = setFreq  261.6256
cs = setFreq 277.1826
d = setFreq  293.6648     
ds = setFreq 311.1270                    
e = setFreq  329.6276                    
f = setFreq  349.2282                    
fs = setFreq 369.9944                    
g = setFreq  391.9954                    
gs = setFreq 415.3047                    
a = setFreq  440.0000
as = setFreq 466.1638  
b = setFreq  493.8833

toFreq :: Pitch -> Freq
toFreq C = c
toFreq Cs = cs
toFreq D = d
toFreq Ds = ds
toFreq E = e
toFreq F = f
toFreq Fs = fs
toFreq G = g
toFreq Gs = gs
toFreq A = a
toFreq As = as
toFreq B = b

shiftOctave :: Int -> Freq -> Freq
shiftOctave n freq = (* 2^^n) . freq

wave :: Float -> Float -> Freq -> [Float]
wave duration vol freq = (* vol) . sin . freq <$> take len [0.0 .. ]
  where len = round $ duration * 48000

silence :: Float -> [Float]
silence duration = take len $ repeat 0.0
  where len = round $ duration * 48000

chord :: Float -> Float -> [Freq] -> [Float]
chord duration vol freqs = 
  case length freqs of 
    0 -> silence duration
    _ -> foldr1 (zipWith (+)) $ wave duration vol <$> freqs

infixl 5 ~|
(~|) = flip shiftOctave

sound = let sop = crotchet c ++ crotchet c ++ crotchet e ++ crotchet e ++ minum g ++ minum g ++
                  minum a ++ crotchet a ++ crotchet a ++ minum g ++ minum e
        in  sop
  where crotchet = wave 1 0.5
        quaver = wave 0.5 0.5
        semiq  = wave 0.25 0.5
        minum  = wave 2 0.5


noteToFreq :: Note -> Freq
noteToFreq (Note pitch (Octave o)) = 
  let shift = o - 4 
  in shiftOctave shift (toFreq pitch)

chordToSound :: Int -> Chord -> B.Builder
chordToSound speed chd = 
  let duration = fromRational $ len chd * 60 / (toRational speed)
      freqs = noteToFreq <$> chordNotes chd
  in foldMap B.floatLE $ chord duration 0.5 freqs

chordToSound' :: Int -> Chord -> [Float]
chordToSound' speed chd = 
  let duration = fromRational $ len chd * 60 / (toRational speed)
      freqs = noteToFreq <$> chordNotes chd
  in chord duration 0.5 freqs

melodyToSound :: Melody -> B.Builder
melodyToSound melody = foldMap (chordToSound $ speed melody) $ tune melody

combineMelodiesToSound :: [Melody] -> B.Builder
combineMelodiesToSound melodies = foldMap B.floatLE $ foldr1 (zipWith (+)) $ processMelody <$> melodies
  where processMelody :: Melody -> [Float]
        processMelody (Melody spd tune) = concat $ chordToSound' spd <$> tune

save :: FilePath -> B.Builder -> IO ()
save filename sound = B.writeFile filename $ B.toLazyByteString sound
