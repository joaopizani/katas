module Parser (parseBoardFromFile) where

import Text.Parsec.Char (char, newline)
import Text.Parsec.Combinator (endBy1, many1)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.ByteString (Parser, parseFromFile)


dead = fmap (const False) (char '.')
alive = fmap (const True) (char 'O')
line = many1 (dead <|> alive)

board :: Parser [[Bool]]
board = line `endBy1` newline

parseBoardFromFile :: FilePath -> IO [[Bool]]
parseBoardFromFile filename =
    do  result <- parseFromFile board filename
        return $ either (error . show) id result
