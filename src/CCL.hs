{-# LANGUAGE OverloadedStrings #-}
module CCL where

import Data.Text (Text)
import Data.Void (Void)
import Data.Map (Map)

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text qualified as T
import Text.Megaparsec.Char.Lexer qualified as L
import qualified Data.Map as M

----------------------------------------
-- Data Types
----------------------------------------

type EntryMap = Map String [EntryValue]

data EntryValue where
    SimpleEntry :: String   -> EntryValue
    NestedEntry :: EntryMap -> EntryValue
    deriving (Show, Eq)

newtype Entry where
    Entry :: { unEntry :: Map String Entry }
          -> Entry
    deriving (Show, Eq)

data KV where
    KV :: { key :: String, val :: String }
       -> KV
    deriving (Show, Eq)

----------------------------------------
-- Simple Operations
----------------------------------------

emptyEntry :: Entry
emptyEntry = Entry M.empty

mergeEntries :: Entry -> Entry -> Entry
mergeEntries (Entry m1) (Entry m2) =
    Entry $ M.unionWith mergeEntries m1 m2

mapToEntry :: EntryMap -> Entry
mapToEntry = Entry . M.map normalizeentries
  where
    normalizeentries = foldl' mergeEntries emptyEntry . map normalizevalues
    normalizevalues  = \case
        SimpleEntry s -> Entry $ M.singleton s emptyEntry
        NestedEntry em -> mapToEntry em

----------------------------------------
-- Parser
----------------------------------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty
-- sc = L.space space1 (L.skipLineComment "/=") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

eqP :: Parser Text
eqP = symbol "="

strip :: String -> String
strip = T.unpack . T.strip . T.pack

emptyP :: Parser String
emptyP = T.unpack <$> string ""

keyP :: Parser String
keyP = strip <$>
    lexeme (many (noneOf (" =":: String))) <?> "key"

keyStart :: Parser ()
keyStart = label "key-start" $ do
    _ <- sc -- optional (some spaceChar)
    _ <- many (noneOf ['='])
    _ <- eqP
    pure ()

lineContent :: Parser String
lineContent = many (noneOf ['\n']) <?> "line-content"

valueP :: Parser String
valueP = label "value" $ do
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

restLinesF :: Parser String
restLinesF = label "restlinefn" $ do
    ls <- many . try $ do
        _ <- newline
        notFollowedBy (try keyStart)
        lineContent
    pure $ concat ls

-- kvP :: Parser KV
-- kvP = label "key value pair" $ do
--     k <- keyP
--     v <- valueP
--     pure $ KV k v

-- kvP' :: Parser KV
-- kvP' = label "key value pair" $ do
--     indentPos <- L.indentLevel
--     k <- keyP
--     _ <- eqP
--     space
--     v <- strip <$> manyTill anySingle newline
--     if null v then do
--         block <- many $ do
--             _ <- L.indentGuard space GT indentPos
--             kvP'
--         pure $ KV k (Block block)
--     else
--         pure $ KV k (Simple v)

-- configP :: Parser [KV]
-- configP = label "config" $ do kvP' `sepBy` newline <* eof

-- config' :: Parser [KV]
-- config' = many kvP' <* eof

----------------------------------------
-- samples
----------------------------------------

sample :: Text
sample = T.pack $
    unlines
        [ "login = LitFill"
        , "name = LitFill The Sanct"
        , "createdAt = 2025-03-13"
        , "timeSpentImplementing = 2d"
        ]

emptyKeys :: Text
emptyKeys = T.pack $
    unlines
        [ "= one"
        , "= two"
        , "= three"
        , "= four"
        ]

emptyVals :: Text
emptyVals = T.pack $
    unlines
        [ "one ="
        , "two ="
        , "three ="
        , "four ="
        ]
