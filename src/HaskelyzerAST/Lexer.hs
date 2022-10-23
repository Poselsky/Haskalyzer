module HaskelyzerAST.Lexer where
import Control.Monad.State

import qualified Text.Parsec.Token as Tok
import Text.Parsec.String (Parser)
import Text.Parsec (ParsecT, SourcePos)
import Control.Monad.Identity (Identity)
import Text.Parsec.Prim
import Text.Parsec.Indent (IndentParser, withPos, withBlock, IndentT)
import Text.Parsec.Char
import qualified Text.Parsec.Language as Tok


haskelyzerLexer :: Tok.GenTokenParser String () (IndentT Identity)
haskelyzerLexer =
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
        ops = ["+","*","-",";", "->" , ":"]
        names = ["let"]

type Name = String
data VarNamePath = VarNamePath 
  {
      varName:: String
    , filePath:: FilePath
  } deriving (Show, Eq)

instance Ord VarNamePath where
  compare a b 
    | filepathLength a > filepathLength b = GT
    | filepathLength b > filepathLength a = LT
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

data HaskelyzerFunction = HaskelyzerFunction Name [Name]
  deriving (Show, Ord, Eq)

data Expr
  = DataExpr DataExpr 
  | BinOp BinOp Expr Expr
  | UnaryOp UnaryOp Expr 
  | FunctionExpr HaskelyzerFunction 
  | Var Name [HaskelyzerFunction]
  | Extern Name [Expr]
  | Schema VarNamePath [DataExpr]
  deriving (Eq, Ord, Show)

data DataExpr = 
    Float Double
    | Int Integer
    | String String
    deriving (Eq, Ord, Show)

type IParser a = IndentParser String () a

integer :: IParser Integer
integer = Tok.integer haskelyzerLexer

float :: IParser Double
float = Tok.float haskelyzerLexer

parens :: IParser a -> IParser a
parens = Tok.parens haskelyzerLexer

commaSep :: IParser a -> IParser [a]
commaSep = Tok.commaSep haskelyzerLexer

semiSep :: IParser a -> IParser [a]
semiSep = Tok.semiSep haskelyzerLexer

identifier :: IParser String
identifier = Tok.identifier haskelyzerLexer

reserved :: String -> IParser ()
reserved = Tok.reserved haskelyzerLexer

reservedOp :: String -> IParser ()
reservedOp = Tok.reservedOp haskelyzerLexer

stringLit:: IParser String
stringLit = Tok.stringLiteral haskelyzerLexer