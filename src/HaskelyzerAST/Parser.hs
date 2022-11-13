module HaskelyzerAST.Parser where

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import HaskelyzerAST.Lexer
import Text.Parsec.Indent
import Text.Parsec.Token (GenTokenParser(stringLiteral))
import HaskelyzerAST.Lexer (stringLit)
import HaskelyzerAST.Schema (schemaParser)
import qualified Data.Text as T
import HaskelyzerAST.Function (variableParser)

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

int :: IParser Expr
int = do
  n <- integer
  return $ DataExpr $ Int (fromInteger n)

floating :: IParser Expr
floating = do
  n <- float
  return $ DataExpr $ Float n

string:: IParser Expr
string = do 
    stringVal <- stringLit
    return $ DataExpr $ String stringVal

expr :: IParser Expr
expr = Ex.buildExpressionParser table factor

factor :: IParser Expr
factor = 
      try schemaParser 
      <|> try variableParser
      <|> parens factor 

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