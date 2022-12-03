module HaskelyzerTemplate.ConcurrentCSV where
import qualified Data.Vector as V
import HaskelyzerAST.Lexer
import qualified Data.Map as M
import Data.Data
import Data.Maybe

type VectorMatrix a = V.Vector (V.Vector a)

type Range = (Int, Int)
class CsvTable t where
    getCols:: (Ord a) => [a] -> t a -> t a
    getRows:: Range -> t a -> t a
    getTableContents:: t a -> VectorMatrix Literal

data ConcurrentCSV a = ConcurrentCSV {
    contents:: VectorMatrix Literal,
    headerMap:: M.Map a Int -- If we have Header in csv then we can map header to int 
} deriving (Eq, Show, Typeable)

createConcurrentCSV:: [[Literal]] -> ConcurrentCSV String
createConcurrentCSV con = ConcurrentCSV {
    contents = V.fromList $ map V.fromList con,
    headerMap = M.empty
}

instance CsvTable ConcurrentCSV where
    getRows (from, to) conCsv@ConcurrentCSV{ contents = c } =
        conCsv {
            contents = V.slice from (to - from) c
            }

    getCols colNames conCsv@ConcurrentCSV{ contents = c, headerMap = m } =
        conCsv {
            contents = let v_colNames = V.fromList colNames in
                (\row -> V.map (\colName -> let index = fromJust $ M.lookup colName m in row V.! index) v_colNames)
                c,
            headerMap = foldr M.delete m colNames
        }

    getTableContents ConcurrentCSV { contents = c } = c