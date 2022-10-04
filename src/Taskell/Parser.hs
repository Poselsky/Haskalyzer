module Taskell.Parser where

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Taskell.Lexer
import Text.Parsec.Indent
import Text.Parsec.Token (GenTokenParser(stringLiteral))
import Taskell.Lexer (stringLit)
import Taskell.Schema (schemaParser)
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

variable :: IParser Expr
variable = do
  var <- identifier
  return $ DataExpr $ Var var 

string:: IParser Expr
string = do 
    stringVal <- stringLit
    return $ DataExpr $ String stringVal

function :: IParser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  body <- expr
  return $ Function name args body

extern :: IParser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: IParser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: IParser Expr
factor = try floating
      <|> try int
      <|> try string
      <|> try extern
      <|> try function
      <|> try call
      <|> try schemaParser 
      <|> variable
      <|> parens expr

defn :: IParser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: IParser a -> IParser a
contents p = do
  r <- p
  eof
  return r

toplevel :: IParser [Expr]
toplevel = many $ do
    def <- defn
    -- reservedOp ";"
    newline
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = 
    runIndentParser (contents expr) () "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = 
    runIndentParser (contents toplevel) () "<stdin>"

