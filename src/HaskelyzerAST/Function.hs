module HaskelyzerAST.Function where
import HaskelyzerAST.Lexer (IParser, identifier, reserved, reservedOp, Expr (Function, Var))
import Text.Parsec (many, sepBy, newline, spaces)
import Control.Monad ( guard )


functionParser:: IParser Expr
functionParser = do
    functionName <- identifier
    args <- identifier `sepBy` spaces
    return $ Function functionName args

variableParser:: IParser Expr
variableParser = do
    reserved "let"
    spaces
    name <- identifier
    spaces
    reservedOp "="
    spaces
    functions <- functionParser `sepBy` (spaces >> reservedOp "->")
    guard (not $ null functions) 

    return $ Var name functions
