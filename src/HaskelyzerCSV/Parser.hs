module HaskelyzerCSV.Parser where

import Text.ParserCombinators.Parsec
    ( ParseError, char, noneOf, eof, sepBy, (<|>), many, try )
import System.IO
import qualified Data.Vector as V
import Control.Monad
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Tok
import Control.Monad.Identity
import HaskelyzerAST.Lexer hiding (float, integer, filePath)
import Text.Parsec (Parsec, runParser, option, oneOf, endOfLine, digit, many1, parserTrace, (<?>), endBy)
import Data.List (intersperse)

type CSVParser a = Parsec String () a -- Stream String, state (), IO transformer, any output

data CSVFile = CSVFile {
    fileContents:: [[Literal]],
    filePath:: String
} deriving (Show, Eq, Ord)

csvFile :: String -> [OptionalColumnNameWithType] -> CSVParser CSVFile
csvFile filePath columnSchema = do
    a <- (line columnSchema) `endBy` eol

    return $ CSVFile {
        fileContents = a,
        filePath = filePath
    }

line:: [OptionalColumnNameWithType] -> CSVParser [Literal]
line columnSchema = do
    row <- sequence $ let i = init columnSchema 
        in (map (\x -> cell x >>= \y -> char ',' >> return y ) i) ++ [cell (last columnSchema)]
    -- row <- cell _ `sepBy` char ','
    parserTrace $ show row
    -- row <- sepBy cell (char ',') 
    unless (length row == length columnSchema) (fail $ "There should be same number of columns as specified per: " ++ show columnSchema)

    return row

cell:: OptionalColumnNameWithType -> CSVParser Literal
-- cell = 
--     (try float) <|> 
--     (try integer) <|> 
--     (many (noneOf ",\n") >>= \x -> return $ String x)

cell (_, CsvFloat) = float <?> "Expected float"
cell (_, CsvInt) = integer <?> "Expected integer"
cell (_, CsvString) = (many (noneOf ",\n") >>= \x -> return $ String x) <?> "Expected string"

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number:: CSVParser [Char]
number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

_integer = plus <|> minus <|> number

integer = fmap (\x -> toLit $ rd x) $ _integer
    where rd       = read :: String -> Integer

float = fmap (\x -> toLit $ rd x) $ _integer <++> decimal <++> exponent
    where rd       = read :: String -> Double
          decimal  = char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> _integer

eol:: CSVParser () 
eol = void (try endOfLine) <|> eof

parseCSV :: String -> String -> [OptionalColumnNameWithType ]-> Either ParseError CSVFile
parseCSV filePath input columnSchema = runParser (csvFile filePath columnSchema) () ("File: " ++ filePath) input
