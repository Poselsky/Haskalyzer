module HaskelyzerAST.Function where
import Text.Parsec (many, sepBy, newline, spaces, between, optional, option,  parserTrace, parserTraced, lookAhead, optionMaybe, notFollowedBy, manyTill, eof, try, char, (<|>), endOfLine, sepEndBy1, sepBy1, anyChar, letter, alphaNum, tab, many1, endBy)
import Control.Monad ( guard, void, (>=>) )
import Text.Parsec.Char (space)
import Debug.Trace (trace)
import Text.Parsec.Indent (indented, block, same, withPos)
import HaskelyzerAST.Lexer
import Text.Parsec (lower)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.Indent.Explicit (indentation)
import HaskelyzerCSV.Parser (eol)

functionParser:: IParser HaskelyzerFunction
functionParser = do
    functionName <- identifier
    optional spaces
    m_hasArgs <- optionMaybe $ lookAhead identifier
    try $ case m_hasArgs of
        Nothing -> return $ HaskelyzerFunction functionName []
        Just _ -> do
            args <- many identifier 
            optional space
            return $ HaskelyzerFunction functionName args

variableParser:: IParser Expr
variableParser = do
    reserved "let"
    name <- spaces >> identifier <* spaces
    args <- sepBy identifier space
    reservedOp "=" >> whiteSp

    functions <- functionChainedParser
    guard (not $ null functions)

    return $ Var name args functions

concurrentParser:: IParser HaskelyzerFunction 
concurrentParser = do
    x <- withPos $ block $ do
        reserved "|"
        try whiteSp
        functionParser `sepBy` reservedOp "->"

    return $ Concurrent x

functionChainedParser:: IParser [HaskelyzerFunction]
functionChainedParser = do
    val <- lookAhead $ optionMaybe concurrentParser
    let applyConcurrentParser = (do a <- concurrentParser; (a :) <$> functionChainedParser;)
    -- parserTrace $ show val
    case val of
        Nothing -> do
            fs <- functionParser `sepBy` reservedOp "->"
            x <- option [] applyConcurrentParser
            return $ fs++x
        Just _ -> applyConcurrentParser