module Parameters ( rate,
                    codomain,
                    concrete ) where

import Melody


-- | The sampling rate, in Hz
rate :: Int
rate = 16000

-- | The codomain for the samples
codomain :: (Int, Int)
codomain = (-40,40)

-- | A Tempo is just an Int, saying how many BPM we have for the song
type Tempo = Int


-- | Make a concrete representation of a musical note. From the
-- pitch and proportional duration give frequency and duration in seconds
concrete :: Tempo -> Note -> (Float, Float)
concrete tempo (Note p d) = (concretePitch p, concreteDuration beat $ fromIntegral d)
    where
        beat = minute / (fromIntegral tempo)
        minute = 60 -- 1min has 60s :)


-- | Convert an abstract (proportional) duration to its concrete duration in seconds
concreteDuration :: Float -> Float -> Float
concreteDuration beat 1  = beat / 1
concreteDuration beat 2  = beat / 2
concreteDuration beat 4  = beat / 4
concreteDuration beat 8  = beat / 8
concreteDuration beat 16 = beat / 16
concreteDuration beat 32 = beat / 32
concreteDuration beat 64 = beat / 64
concreteDuration beat _  = beat / 1


-- | Get the frequency of a pitch
concretePitch :: Pitch -> Float
concretePitch C4  = 261.626
concretePitch C4s = 277.183
concretePitch D4  = 293.665
concretePitch D4s = 311.127
concretePitch E4  = 329.628
concretePitch F4  = 349.228
concretePitch F4s = 369.994
concretePitch G4  = 391.995
concretePitch G4s = 415.305
concretePitch A4  = 440.000
concretePitch A4s = 466.164
concretePitch B4  = 493.883
concretePitch C5  = 523.251
concretePitch C5s = 554.365
concretePitch D5  = 587.330
concretePitch D5s = 622.254
concretePitch E5  = 659.255
concretePitch F5  = 698.456
concretePitch F5s = 739.989
concretePitch G5  = 783.991
concretePitch G5s = 830.609
concretePitch A5  = 880.000
concretePitch A5s = 932.328
concretePitch B5  = 987.767
concretePitch P4  = 0.0 -- the frequency of a pause is zero
