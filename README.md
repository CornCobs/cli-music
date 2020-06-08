# cli-music

cli-music is a simple utility and music language that reads a simple text format and outputs a raw PCM audio bytestream.
The raw file can then be played via ffplay or converted into other formats via ffmpeg. This is obviously a hobby project, strictly for fun.

## Usage:

First write your music in a text file, save it e.g. `tune1.txt`

Now perform the following:

```
$ cli-music tune1.txt tune1.raw
```

tune1.raw is encoded as 32-bit floating point, little endian. You can play it via

```
$ ffplay -f f32le -ar 48000 tune1.raw
```

Or convert to MP3 via

```
$ ffmpeg -f f32le -ar 48000 -i tune1.raw tune1.mp3
```

## Music syntax

The syntax is very simple and loosely inspired by [alda](https://alda.io/)
1. Comments: Comments start with '#', and everything until the end of the line is ignored
2. Melody: To write a melody, first specify a speed (in bpm), the octave the melody starts in, and then
   the list of chords/notes of the tune surrounded in braces `{}`. You can write multiple melodies in a file for multiple part harmonies - the melodies will be played together.
3. Notes: Each note in a melody is separated by spaces or newlines. The syntax looks like this:
   `C2` - C is the note, and 2 is the length of the note (a minum). Sharps and flats are denoted by + and - after the note, before the length, e.g. `C+2` is a minum of C#. If the length is left out, it defaults to a crotchet. Fractional lengths are denoted by `/n`, e.g. `A/2` is a quaver A and `D-/4` is a semiquaver D-flat. A period `.` after the duration indicates the duration length is 1.5x longer. You can have multiple `.` and multiple `+` and `-`.
4. Chords: Instead of just having a single note, you can specify a chord of notes as well. A chord is a group of notes in 
   parentheses, with the length after the closing parenthesis - e.g. `(C E G)2`
5. Changing octave: You can change the octave between notes by using `>` to increase the octave and `<` to decrease it. 
   You can also change the octave within a chord, e.g. `(C E G > C)1.`. Take note that changes within a chord are local to the chord and do not change the octave permanently, while changes outside a chord affect all subsequent notes. If you want to change the octave for a single note you can thus either change it back immediately, e.g. `> C <` increases the octave for the C note, then decreases it again, or wrap the note in a chord, e.g. `(> C)`, which will not affect subsequent notes.

Thats about all of it! 

Sample score:  
```
# Soprano part
speed: 80 
octave: 4
{ 
  C C E E G2 G2 A2 A A G2 E2 # Comment within the score!
  G _/2 G/2 G G (> C)2 B G D G A. G/2 G4
}

# Alto
speed: 80
octave: 4
{
  < G G > C C < B > D C < B A B > C D E2 C2
  D D E D C D D E D < B > C. < B/2 B4
}

# Tenor
speed: 80
octave: 3
{
  E E C C D F E G F G A B (> C) G G2
  G G G G E F+ G G B G F+. G/2 G2 F2
}

# Bass
speed: 80
octave: 3
{
  C C < A A G2 (> C)2 F2 > F F C2 C2
  < B B (> C) B A2 B > C D D D. < G/2 G4 
}
```