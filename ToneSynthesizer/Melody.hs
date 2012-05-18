module Melody ( score,
                Pitch(..),
                Note(..) ) where

import Text.Parsec
import Text.Parsec.ByteString (Parser)
import Data.Maybe (maybe)
import Data.Char (toUpper)
import Control.Monad (sequence, liftM2)


-- | Datatype definition for the pitch of a musical note. Two octaves
data Pitch = C4 | C4s | D4 | D4s | E4 | F4 | F4s | G4 | G4s | A4 | A4s | B4 |
             C5 | C5s | D5 | D5s | E5 | F5 | F5s | G5 | G5s | A5 | A5s | B5 |
             P4 -- pause
    deriving (Eq, Ord, Enum, Read)


-- | Parsing a pitch
name = let singleton = (:[])  in  fmap (singleton . toUpper) $ oneOf "cdefgabp"
sharp = fmap (maybe "" (const "s")) $ optionMaybe (char '#')
quote = fmap (maybe "4" (const "5")) $ optionMaybe (char '\'')

pitch :: Parser Pitch
pitch = do [n,s,q] <- sequence [name, sharp, quote]
           return $ read (concat [n,q,s])



-- | Showing a pitch
instance Show Pitch where
    show C4  = "c"
    show C4s = "c#"
    show D4  = "d"
    show D4s = "d#"
    show E4  = "e"
    show F4  = "f"
    show F4s = "f#"
    show G4  = "g"
    show G4s = "g#"
    show A4  = "a"
    show A4s = "a#"
    show B4  = "b"
    show C5  = "c'"
    show C5s = "c#"
    show D5  = "d'"
    show D5s = "d#'"
    show E5  = "e'"
    show F5  = "f'"
    show F5s = "f#'"
    show G5  = "g'"
    show G5s = "g#'"
    show A5  = "a'"
    show A5s = "a#'"
    show B5  = "b'"
    show P4  = "p"



-- | Musical note datatype
data Note = Note Pitch Int
    deriving (Eq)

-- | Parsing musical notes
duration = fmap read $ many1 digit

note :: Parser Note
note = liftM2 Note pitch duration


-- | Showing musical notes
instance Show Note where
    show (Note pitch duration) = show pitch ++ show duration



-- | Parsing a score
white = skipMany1 (space <|> newline)
separator = char ',' >> white

score :: Parser [Note]
score = do notes <- note `sepBy1` separator
           optional newline >> eof
           return notes
