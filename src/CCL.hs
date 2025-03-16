{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module CCL where

import Data.Text (Text)
import Data.Void (Void)
import Data.Map (Map)

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text qualified as T
import Text.Megaparsec.Char.Lexer qualified as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad (void, (>=>))
import qualified Data.Text.IO as TIO

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

insertKV :: EntryMap -> KV -> EntryMap
insertKV emap (KV k v) =
    let newEntry = either
                       (const $ SimpleEntry v)
                       (NestedEntry . kvsToEntryMap)
                       (parseValue v)
     in M.alter (\mlist -> Just (newEntry : fromMaybe [] mlist)) k emap

kvsToEntryMap :: [KV] -> EntryMap
kvsToEntryMap = foldl' insertKV M.empty

parseValue :: String -> Either a0 b0
parseValue = undefined

kvsToEntry :: [KV] -> Entry
kvsToEntry = mapToEntry . kvsToEntryMap

-- Pretty Print
ppEntry :: Entry -> String
ppEntry = concat . fmtentry 0
  where
    fmtentry :: Int -> Entry -> [String]
    fmtentry indent (Entry m) =
        M.foldrWithKey (\k v acc ->
            indentline indent k :
            fmtentry (indent + 4) v ++ acc) [] m
    indentline n keyname =
        replicate n ' ' ++ keyname ++ " =\n"

----------------------------------------
-- DSL
----------------------------------------

mkEntry :: String -> Entry
mkEntry = Entry . (`M.singleton` emptyEntry)

mkEntryKv :: String -> String -> Entry
mkEntryKv k = Entry . M.singleton k . mkEntry

mergeEntryList :: [Entry] -> Entry
mergeEntryList = foldl' mergeEntries emptyEntry

mkNested :: String -> [Entry] -> Entry
mkNested k = Entry . M.singleton k . mergeEntryList

infixl 8 |=
(|=) :: String -> String -> Entry
(|=) = mkEntryKv

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

skipBlanks :: Parser ()
skipBlanks = skipMany (hspace <|> void newline)

keyP :: Parser String
keyP = strip <$>
    lexeme (many (noneOf ("=":: String))) <?> "key"

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

----------------------------------------
-- nested experiment
----------------------------------------

-- pewaris =
--     ibn = 2
--     ab  = 1
--     um  = 1

-- KV { key = "pewaris", value = "    ibn = 2\n    ab  = 1\n    um  = 1"}

-- take key
-- take =
-- check not \n
--   t -> take until \n
--   f -> take \n
--        check indented
--          f -> val is ""
--          t -> take until \n
--               check indented :loop

nestedKVP :: Parser KV
nestedKVP = do
    k   <- many (satisfy (/= '='))
    _   <- char '='
    mnl <- optional (lookAhead anySingle)
    v   <- case mnl of
        Nothing -> error "eof"
        Just '\n' -> parseindented
        Just _ -> many (satisfy (/= '\n'))
    pure $ KV (strip k) v
  where
    parseindented = do
        _  <- newline
        ls <- fmap strip <$> some indentedline'
        pure (unlines ls)

indentedlines :: Parser [String]
indentedlines =
    manyTill indentedline' (lookAhead (void nonindentedline) <|> eof)

indentedline' :: Parser String
indentedline' = do
    spcs <- some (char ' ')
    line <- manyTill anySingle (try (void newline) <|> eof)
    pure $ spcs ++ line

nonindentedline :: Parser String
nonindentedline = do
    notFollowedBy (char ' ' <|> char '\t')
    manyTill anySingle (try (void newline) <|> eof)

----------------------------------------
-- END nested experiment
----------------------------------------

valP :: Int -> Parser String
valP minIndent = do
    -- skipMany hspace
    frst <- many (satisfy (/= '\n'))
    rest <- many $ try parserest
    pure $ frst ++ concat rest
  where
    indentedline spcs = do
        line <- many (satisfy (/= '\n'))
        _ <- optional newline
        pure $ "\n" ++ spcs ++ line
    parserest = do
        _    <- newline
        spcs <- many (char ' ' <|> char '\t')
        let n = length spcs
        next <- optional (lookAhead anySingle)
        if | n == 0 && next == Just '\n' -> newline >> pure "\n"
           |        n > minIndent        -> indentedline spcs
           |          otherwise          -> indentedline ""

restLinesF :: Parser String
restLinesF = label "restlinefn" $ do
    ls <- many . try $ do
        _ <- newline
        notFollowedBy (try keyStart)
        lineContent
    pure $ concat ls

kvP' :: Parser KV
kvP' = do
    key' <- keyP
    val' <- valueP
    pure $ KV key' val'

kvP :: Int -> Parser KV
kvP minIndent = do
    -- key' <- many (satisfy (/= '\n'))
    key' <- keyP
    -- skipBlanks
    _ <- eqP
    val' <- valP minIndent
    -- skipBlanks
    pure $ KV key' (strip val')

kvsP :: Parser [KV]
kvsP = skipBlanks *> many (kvP 0)

kvsP' :: Parser [KV]
kvsP' =  many kvP'

nested :: Parser [KV]
nested = do
    m <- optional (lookAhead anySingle)
    case m of
        Nothing -> pure []
        Just '\n' -> do
            _ <- newline
            spaces <- many (char ' ' <|> char '\t')
            let indent = length spaces
            many (kvP indent)
        Just _ -> pure <$> kvP 0

parseKVs :: Text -> Either (ParseErrorBundle Text Void) [KV]
parseKVs = parse (kvsP <* eof) ""

parseNested :: Text -> Either (ParseErrorBundle Text Void) [KV]
parseNested = parse (nested <* eof) ""

parseFile :: FilePath -> IO ()
parseFile = TIO.readFile >=> parseTest (kvsP' <* eof)

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
