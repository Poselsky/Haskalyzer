module HaskelyzerAST.Function where
import HaskelyzerAST.Lexer (IParser, identifier, reserved, reservedOp, Expr (FunctionExpr, Var), HaskelyzerFunction (HaskelyzerFunction))
import Text.Parsec (many, sepBy, newline, spaces)
import Control.Monad ( guard )


functionParser:: IParser HaskelyzerFunction 
functionParser = do
    functionName <- identifier
    args <- identifier `sepBy` spaces
    return $ HaskelyzerFunction functionName args

variableParser:: IParser Expr
variableParser = do
    reserved "let"
    spaces
    name <- identifier
    spaces >> reservedOp "=" >> spaces
    functions <- functionParser `sepBy` (spaces >> reservedOp "->")  >>= \x -> return $ map FunctionExpr x
    guard (not $ null functions) 

    return $ Var name functions
