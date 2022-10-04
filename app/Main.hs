{-# LANGUAGE OverloadedStrings #-}
module Main where

import Taskell.Parser
import Text.Parsec
import Taskell.Lexer
import Text.Parsec.Indent
import Taskell.Schema (schemaParser)
import qualified Text.Parsec.Token as Tok
import System.Environment


input_text = unlines 
    [
        "file:",
        "  String",
        "  Int"
    ]


toplevelP :: IParser [Expr]
toplevelP = do 
    Tok.whiteSpace taskellLexer
    def <- many $ do 
        s <- try schemaParser 
        try $ many newline
        return s
    -- reservedOp ";"
    eof
    return def

parseToplevelP :: String -> Either ParseError [Expr]
parseToplevelP input = 
    runIndent $ runParserT toplevelP () "<stdin>" input

main :: IO ()
main = do
    t <- readFile "./testFiles/schema.tkl"
    print $ parseToplevelP t 
