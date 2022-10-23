module HaskelyzerAST.Function where
import HaskelyzerAST.Lexer (IParser, identifier, reserved, reservedOp, Expr (FunctionExpr, Var), HaskelyzerFunction (HaskelyzerFunction))
import Text.Parsec (many, sepBy, newline, spaces, between, optional, option,  parserTrace, parserTraced, lookAhead, optionMaybe, notFollowedBy, manyTill, eof, try, char, (<|>), endOfLine, sepEndBy1, sepBy1, anyChar, letter, alphaNum)
import Control.Monad ( guard )
import Text.Parsec.Char (space)
import Debug.Trace (trace)

functionParser:: IParser HaskelyzerFunction 
functionParser = do
    functionName <- identifier 
    spaces
    x <- optionMaybe $ lookAhead letter 
    case x of
      Nothing -> 
            return $ HaskelyzerFunction functionName []
      Just x1 -> do
            args <- identifier `sepBy1` spaces 
            return $ HaskelyzerFunction functionName args

variableParser:: IParser Expr
variableParser = do
    reserved "let"
    name <- between spaces spaces identifier
    reservedOp "=" >> spaces 

    functions <- many $ try pipeline <|> functionParser
    guard (not $ null functions) 

    return $ Var name functions

        where 
            pipeline = do
                f <- functionParser 
                between spaces spaces $ reservedOp "->" 
                return f