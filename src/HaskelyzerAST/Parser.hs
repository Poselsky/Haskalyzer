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

expr :: IParser Expr
expr = Ex.buildExpressionParser table factor


string:: IParser Expr
string = do 
    stringVal <- stringLit
    return $ DataExpr $ String stringVal

factor :: IParser Expr
factor = try floating
      <|> try int
      <|> try string
      <|> try schemaParser 
      <|> parens expr

contents :: IParser a -> IParser a
contents p = do
  r <- p
  eof
  return r

toplevel :: IParser [Expr]
toplevel = many $ do
    def <- expr 
    -- reservedOp ";"
    newline
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = 
    runIndentParser (contents expr) () "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = 
    runIndentParser (contents toplevel) () "<stdin>"
