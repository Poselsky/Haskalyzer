module HaskelyzerAST.Function where
import HaskelyzerAST.Lexer (IParser, identifier, reserved, reservedOp, Expr (FunctionExpr, Var), HaskelyzerFunction (HaskelyzerFunction), whiteSp)
import Text.Parsec (many, sepBy, newline, spaces, between, optional, option,  parserTrace, parserTraced, lookAhead, optionMaybe, notFollowedBy, manyTill, eof, try, char, (<|>), endOfLine, sepEndBy1, sepBy1, anyChar, letter, alphaNum, tab)
import Control.Monad ( guard, void )
import Text.Parsec.Char (space)
import Debug.Trace (trace)

tabOrSpaces = many (tab <|> char ' ')

functionParser:: IParser HaskelyzerFunction
functionParser = do
    functionName <- identifier
    try tabOrSpaces
    args <- identifier `sepBy` tabOrSpaces
    return $ HaskelyzerFunction functionName args

variableParser:: IParser Expr
variableParser = do
    reserved "let"
    name <- between tabOrSpaces tabOrSpaces identifier
    reservedOp "=" >> tabOrSpaces

    functions <- functionParser `sepBy1` between tabOrSpaces tabOrSpaces (reservedOp "->")
    guard (not $ null functions)

    return $ Var name functions