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
import Text.Parsec (Parsec, runParser, option, oneOf, endOfLine, digit, many1)

type CSVParser a = Parsec String () a -- Stream String, state (), IO transformer, any output

data CSVFile = CSVFile {
    fileContents:: [[Literal]],
    filePath:: String
} deriving (Show, Eq, Ord)

csvFile :: String -> CSVParser CSVFile
csvFile filePath = do
    a <- line `sepBy` eol 

    return $ CSVFile {
        fileContents = a,
        filePath = filePath
    }

line:: CSVParser [Literal]
line = sepBy cell (char ',') 

cell:: CSVParser Literal 
cell = 
    (try float) <|> 
    (try integer) <|> 
    (many (noneOf ",\n") >>= \x -> return $ String x)

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

eol:: CSVParser Char 
eol = endOfLine 

parseCSV :: String -> String -> Either ParseError CSVFile
parseCSV filePath = runParser (csvFile filePath) () ("File: " ++ filePath)
