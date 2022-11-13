module HaskelyzerAST.Schema where
import HaskelyzerAST.Lexer
    ( IParser,
      DataExpr(..),
      Expr(SchemaExpr),
      VarNamePath(..),
      identifier, reservedOp, reserved, stringLit, Schema (Schema) )
import Text.Parsec (between)
import Text.Parsec.Char (char)
import Text.Parsec.Indent (sameOrIndented, indentBraces, withBlock, block, indented, withPos, checkIndent)
import Text.Parsec.Combinator
    ( choice, between, manyTill, optional )
import Text.Parsec.Prim ( many )
import Text.Parsec (spaces, newline)
import Text.Parsec (alphaNum, endOfLine, anyChar)
import System.FilePath (isValid)
import Control.Monad (guard, void)
import Debug.Trace (trace)
import Text.Parsec.Indent.Explicit (indentation)
import Text.Parsec (space)
import Text.Parsec (string)


schemaParser:: IParser Expr
schemaParser = do
    char '{'
    optional $ many newline

    fileSchema <- withBlock
        (\filename rowTypes -> SchemaExpr $ Schema filename $ map rowTypeToDataExpr rowTypes)
        letVarNameParser
        (
            do
                l <- choice $ map string ["Int", "Float", "String"] 
                spaces
                return l
        )

    optional $ many newline
    char '}'
    return fileSchema

letVarNameParser:: IParser VarNamePath
letVarNameParser = do
    indentation >> spaces

    reserved "let"
    varName <- between spaces spaces identifier 
    reservedOp "="
    spaces
    fileName <- stringLit
    spaces
    reservedOp ":"

    guard $ isValid fileName

    return VarNamePath { varName = varName, filePath = fileName } 


rowTypeToDataExpr:: String -> DataExpr
rowTypeToDataExpr "Int" = Int 0
rowTypeToDataExpr "Float" = Float 0
rowTypeToDataExpr "String" = String ""
rowTypeToDataExpr rowType = error $ "This datatype can't exist: " ++ rowType


