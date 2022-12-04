module HaskelyzerAST.Schema where
import HaskelyzerAST.Lexer
    ( IParser,
      Expr(SchemaExpr),
      VarNamePath(..),
      identifier, reservedOp, reserved, stringLit, Schema (Schema), parens, braces, CsvDataType (..) )
import Text.Parsec.Char (char)
import Text.Parsec.Indent (sameOrIndented, indentBraces, withBlock, block, indented, withPos, checkIndent)
import Text.Parsec.Combinator
    ( choice, between, manyTill, optional )
import Text.Parsec.Prim ( many )
import Text.Parsec (alphaNum, endOfLine, anyChar, spaces, newline, string, optionMaybe)
import System.FilePath (isValid)
import Control.Monad (guard, void, unless)
import Debug.Trace (trace)
import Text.Parsec.Indent.Explicit (indentation)
import Data.Maybe (isJust, isNothing)

schemaParser:: IParser Expr
schemaParser = do
    schema@(Schema varPath vals) <- _schemaParser
    let areAllColumnsJustOrNothing = all (\(x,_) -> isJust x || isNothing x) vals
    unless areAllColumnsJustOrNothing $ fail "If CSV file has header names, then all columns must have a name"

    return $ SchemaExpr schema

_schemaParser:: IParser Schema
_schemaParser =
    braces (do
        optional $ many newline

        fileSchema <- withBlock
            (\filename rowTypes -> Schema filename $ map (\(n, t) -> (n, rowTypeToDataExpr t)) rowTypes)
            letVarNameParser
            dataTypeColumn

        optional $ many newline
        return fileSchema
    )

dataTypeColumn:: IParser (Maybe String, String)
dataTypeColumn = do
    let choiceOfType = choice $ map string ["Int", "Float", "String"]
    let m_columnName = choiceOfType >>= \x -> do spaces; return (Nothing, x)

    m_columnNameWithType <- optionMaybe $ parens (do
            n <- identifier
            reservedOp ","
            c <- choiceOfType
            spaces
            return (Just n,c)
        )

    maybe m_columnName return m_columnNameWithType

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


rowTypeToDataExpr:: String -> CsvDataType 
rowTypeToDataExpr "Int" = CsvInt 
rowTypeToDataExpr "Float" = CsvFloat 
rowTypeToDataExpr "String" = CsvString
rowTypeToDataExpr rowType = error $ "This datatype can't exist: " ++ rowType


