module HaskelyzerCSV.Parser where

import Text.ParserCombinators.Parsec
    ( ParseError, char, noneOf, eof, sepBy, (<|>), many, try )
import System.IO
import Text.Parsec (ParsecT, runParserT)
import qualified Data.Vector as V
import Control.Monad

type CSVParser a = ParsecT String () IO a -- Stream String, state (), IO transformer, any output


data CSVFile = CSVFile {
    fileContents:: [[String]],
    filePath:: String
} deriving (Show, Eq, Ord)

csvFile :: String -> CSVParser CSVFile
csvFile filePath = do
    a <- line `sepBy` eol >>= \x -> eofOrEol >> return x

    return $ CSVFile {
        fileContents = a,
        filePath = filePath
    }

line:: CSVParser [String]
line = sepBy cell (char ',')

cell:: CSVParser String
cell = many (noneOf ",\n")

eol:: CSVParser Char
eol = char '\n'

eofOrEol:: CSVParser ()
eofOrEol = eof <|> void eol

parseCSV :: String -> String -> IO (Either ParseError CSVFile)
parseCSV filePath = runParserT (csvFile filePath) () ("File: " ++ filePath)
