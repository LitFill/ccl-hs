{-# LANGUAGE OverloadedStrings #-}
module CCL where

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text qualified as T
import Text.Megaparsec.Char.Lexer qualified as L

----------------------------------------
-- Data Types
----------------------------------------

data KV where
    KV :: {key :: String, value :: String}
        -> KV
    deriving (Show, Eq)

----------------------------------------
-- Parser
----------------------------------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "/=") Text.Megaparsec.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

eqP :: Parser Text
eqP = symbol "="

strip :: String -> String
strip = T.unpack . T.strip . T.pack

keyP :: Parser String
keyP = strip <$>
    lexeme (many (noneOf ("=":: String))) <?> "key"

valueP :: Parser String
valueP = strip <$>
    lexeme (many printChar) <?> "value"

nonChar :: Char -> Parser Char
nonChar c =
    L.charLiteral >>= \c' ->
        if c' == c
            then fail $ "Unexpected " ++ show c
            else pure c'

keyP' :: Parser String
keyP' = strip <$> many (try $ nonChar '=')

valueP' :: Parser String
valueP' = strip <$> many (try $ nonChar '\n')

kvP :: Parser KV
kvP = do
    k <- keyP
    _ <- eqP
    space
    v <- valueP
    space
    pure $ KV k v

configP :: Parser [KV]
configP = kvP `sepEndBy` newline <* eof

sample :: Text
sample = T.pack $
    unlines
        [ "login = LitFill"
        , "name = LitFill The Sanct"
        , "createdAt = 2025-03-13"
        , "timeSpentImplementing = 2d"
        ]
