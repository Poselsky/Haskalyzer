module Taskell.Lexer where
import Control.Monad.State

import qualified Text.Parsec.Token as Tok
import Text.Parsec.String (Parser)
import Text.Parsec (ParsecT, SourcePos)
import Control.Monad.Identity (Identity)
import Text.Parsec.Prim
import Text.Parsec.Indent (IndentParser, withPos, withBlock, IndentT)
import Text.Parsec.Char
import qualified Text.Parsec.Language as Tok


taskellLexer :: Tok.GenTokenParser String () (IndentT Identity)
taskellLexer =
  Tok.makeTokenParser Tok.emptyDef 
    { 
        Tok.commentStart = "#{",
        Tok.commentEnd = "}#",
        Tok.commentLine = "##",
        Tok.reservedOpNames = ops,
        Tok.reservedNames = names,
        Tok.identStart = letter,
        Tok.identLetter = letter,
        Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.nestedComments = True,
        Tok.caseSensitive = True
    }
    where
        ops = ["+","*","-",";"]
        names = ["def","extern"]

type Name = String
data VarNamePath = VarNamePath 
  {
      varName:: String
    , filePath:: FilePath
  } deriving (Show, Eq)

instance Ord VarNamePath where
  compare a b 
    | filepathLength a > filepathLength b = LT
    | filepathLength b > filepathLength a = GT
    | otherwise                           = EQ
    where
      filepathLength = length . filePath

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

data UnaryOp
    = Not
    | Nroot -- Like SquareRoot but N 
    | Exponent
    deriving (Eq, Ord, Show)

data Expr
  = DataExpr DataExpr 
  | BinOp BinOp Expr Expr
  | UnaryOp UnaryOp Expr 
  | Call Name [Expr]
  | Function Name [Expr] Expr
  | Extern Name [Expr]
  | Schema VarNamePath [DataExpr]
  deriving (Eq, Ord, Show)

data DataExpr = 
    Float Double
    | Int Integer
    | Var String
    | String String
    deriving (Eq, Ord, Show)

type IParser a = IndentParser String () a

integer :: IParser Integer
integer = Tok.integer taskellLexer

float :: IParser Double
float = Tok.float taskellLexer

parens :: IParser a -> IParser a
parens = Tok.parens taskellLexer

commaSep :: IParser a -> IParser [a]
commaSep = Tok.commaSep taskellLexer

semiSep :: IParser a -> IParser [a]
semiSep = Tok.semiSep taskellLexer

identifier :: IParser String
identifier = Tok.identifier taskellLexer

reserved :: String -> IParser ()
reserved = Tok.reserved taskellLexer

reservedOp :: String -> IParser ()
reservedOp = Tok.reservedOp taskellLexer

stringLit:: IParser String
stringLit = Tok.stringLiteral taskellLexer

add:: IParser Expr
add = do
  string "add"
  return $ Function "add" [DataExpr $ Int 0] $ DataExpr $ Int 0