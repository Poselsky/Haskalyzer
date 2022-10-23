module Main where

import HaskelyzerAST.Parser
import Text.Parsec
import HaskelyzerAST.Lexer
import Text.Parsec.Indent
import HaskelyzerAST.Schema (schemaParser)
import qualified Text.Parsec.Token as Tok
import System.Environment
import Language.Haskell.Interpreter (Interpreter, loadModules)
import HaskelyzerInterpreter.InterpreterState (runInterpreterRWS)
import HaskelyzerAST.Function (functionParser, variableParser)

toplevelP :: IParser [Expr]
toplevelP = do 
    def <- many $ do 
        try $ many newline
        s <- expr 
        try $ many newline
        return s
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
    let ast = parseToplevelP t

    print ast

    case ast of Right e -> runInterpreterRWS e 
                Left e -> return () 
    return ()
