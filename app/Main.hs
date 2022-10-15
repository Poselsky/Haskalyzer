{-# LANGUAGE OverloadedStrings #-}
module Main where

import HaskelyzerAST.Parser
import Text.Parsec
import HaskelyzerAST.Lexer
import Text.Parsec.Indent
import HaskelyzerAST.Schema (schemaParser)
import qualified Text.Parsec.Token as Tok
import System.Environment
import Language.Haskell.Interpreter (Interpreter, loadModules)

toplevelP :: IParser [Expr]
toplevelP = do 
    Tok.whiteSpace haskelyzerLexer
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

haskellScriptBackingFile:: FilePath -> Interpreter ()
haskellScriptBackingFile pathToBackingFile = do
    loadModules [pathToBackingFile]

main :: IO ()
main = do
    t <- readFile "./testFiles/schema.tkl"
    print $ parseToplevelP t 
