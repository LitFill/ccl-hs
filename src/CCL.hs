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
    lexeme (many (noneOf ("=\n":: String))) <?> "key"

keyStart :: Parser ()
keyStart = do
    _ <- sc
    _ <- many (noneOf ['=', '\n'])
    _ <- eqP
    pure ()

lineContent :: Parser String
lineContent = many (noneOf ['\n'])

valueP :: Parser String
valueP = do
    _         <- eqP
    fstLine   <- lineContent
    restLines <- many . try $ do
        _ <- newline
        notFollowedBy (try keyStart)
        lineContent
    pure .
        T.unpack .
        T.strip .
        T.intercalate "\n" $
        T.pack <$> (fstLine : restLines)

kvP :: Parser KV
kvP = do
    k <- keyP
    v <- valueP
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
