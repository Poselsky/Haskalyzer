module HaskelyzerCSV.Parser where

import Text.ParserCombinators.Parsec
import System.IO
import Text.Parsec (ParsecT, runParserT)
import qualified Data.Vector as V 

type CSVParser a = ParsecT String () IO a -- Stream String, state (), IO transformer, any output

type VectorMatrix a = V.Vector (V.Vector a)

data CSVFile = CSVFile {
    fileContents:: VectorMatrix String,
    filePath:: String
} deriving (Show, Eq, Ord)

csvFile :: String -> CSVParser CSVFile 
csvFile filePath = do 
    a <- endBy line eol

    return $ CSVFile {
        fileContents = V.fromList $ map V.fromList a,
        filePath = filePath
    }

line:: CSVParser [String]
line = sepBy cell (char ',')

cell:: CSVParser String 
cell = many (noneOf ",\n")

eol:: CSVParser Char
eol = char '\n'

parseCSV :: String -> String -> IO (Either ParseError CSVFile)
parseCSV filePath = runParserT (csvFile filePath) () ("File: " ++ filePath)
