module Main where

import CCL

import System.Environment (getArgs)

handleArgs :: [String] -> IO ()
handleArgs = \case
    ["parse", fname] -> parseFileWith onlyparseindented fname
    _                -> putStrLn "usage: <> parse [file]"

onlyparseindented :: Parser [String]
onlyparseindented = manyTill (try indentedline') eof

main :: IO ()
main = getArgs >>= handleArgs
