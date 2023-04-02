module Parsing.Helpers where

import HaskelyzerAST.Lexer (Expr, IParser)
import Text.Parsec (ParseError)
import Text.Parsec.Indent (runIndent)
import Text.Parsec.Prim (runParserT)


indentParser:: IParser a -> String -> Either ParseError a 
indentParser parsingFunction input = 
    runIndent $ runParserT parsingFunction () "Test" input