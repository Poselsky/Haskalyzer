module HaskelyzerHelpers.MatrixHelpers where
import Data.Sequence (mapWithIndex, Seq)

type RowColumn = (Int, Int)
doubleMapIndex:: (RowColumn -> a -> b) -> Seq (Seq a) -> Seq (Seq b)
doubleMapIndex f = mapWithIndex (\rowIndex -> mapWithIndex (\columnIndex -> f (rowIndex, columnIndex)))
