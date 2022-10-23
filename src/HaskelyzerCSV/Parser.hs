module HaskelyzerCSV.Parser where

import Text.ParserCombinators.Parsec
import System.IO
import Text.Parsec (ParsecT, runParserT)

type CSVParallelParser a = ParsecT String () IO a -- Stream String, state (), IO transformer, any output

csvFile :: CSVParallelParser [[String]]
csvFile = endBy line eol

line:: CSVParallelParser [String]
line = sepBy cell (char ',')

cell:: CSVParallelParser String 
cell = many (noneOf ",\n")

eol:: CSVParallelParser Char
eol = char '\n'

parseCSV :: String -> String -> IO (Either ParseError [[String]])
parseCSV filePath input = runParserT csvFile () filePath input
