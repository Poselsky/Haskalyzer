{-# LANGUAGE TemplateHaskell #-}
module HaskelyzerTemplate.MainTemplate where
import Language.Haskell.TH
    ( mkName,
      Quote(newName),
      Exp(LamE, VarE, AppE, InfixE, RecConE, ListE, LitE),
      Q,
      Dec (ValD),
      runIO,
      Pat(VarP),
      Clause(Clause),
      Name,
      Dec(FunD),
      Body(NormalB), Lit (StringL) )
import HaskelyzerAST.Parser
import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering))
import GHC.IO.Handle.FD (stdout, stderr)
import HaskelyzerAST.Lexer hiding (Name)
import Control.Monad ((>=>), unless)
import qualified GHC.Base
import HaskelyzerCSV.Parser (parseCSV, CSVFile (CSVFile))
import Data.Either (fromRight, isRight)
import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Data.List (intersect, intersectBy)
import Data.Data (Typeable, typeOf)
import qualified Data.Vector as V

type VectorMatrix a = V.Vector (V.Vector a)

generateSDL:: FilePath -> Q [Dec]
generateSDL filePath = do
    runIO $ hSetBuffering stdout NoBuffering -- I want prints during compilation
    runIO $ hSetBuffering stderr NoBuffering -- I want prints during compilation

    t <- runIO $ readFile filePath
    let ast = parseToplevelP t

    runIO $ print ast
    case ast of
      Right exs -> mapM astExprToDec exs
      Left pe -> error $ show pe

astExprToDec:: Expr -> Q Dec
astExprToDec (Var n haskFunctions ) = do
    let name = mkName n
    haskFunctionsAsExp <- mapM haskelyzerFunctionToExpr haskFunctions

    return $ FunD name [Clause [] (NormalB $ composed haskFunctionsAsExp) []]
    where
        composed:: [Exp] -> Exp
        composed (fa:fb:fs) = let composeName = mkName "." in
            foldl (\acc x -> InfixE (Just x) (VarE composeName) (Just acc)) (InfixE (Just fb) (VarE composeName) (Just fa)) fs
        composed [fa] = fa
        composed [] = error "Variable can't be empty"

astExprToDec (SchemaExpr (Schema (VarNamePath var path) dataExpr)) = do

    csvFileContents <- runIO $ readFile path
    either_csv <- runIO $ parseCSV path csvFileContents

    case either_csv of 
      Left pe -> fail $ "Can't parse csv: " ++ show pe
      Right (CSVFile contents _) -> do 

        let tableName = mkName var 
            csvDataTypeName = mkName $ show 'ConcurrentCSV 
            contentsName = mkName "contents"  -- TODO: use reify to check ConcurrentCSV record type
            headerMapName = mkName "headerMap"  -- TODO: use reify to check ConcurrentCSV record type
            createName = mkName "createConcurrentCSV"

            vectorBuilderHelper x = ListE $ map (\y -> LitE $ StringL y) x

        return $ FunD 
            tableName
            [Clause 
                []
                (NormalB $ AppE (VarE createName) (ListE $ map vectorBuilderHelper contents))
                []
            ]

astExprToDec e = error ""

haskelyzerFunctionToExpr:: HaskelyzerFunction -> Q Exp
haskelyzerFunctionToExpr (HaskelyzerFunction name args) = do
        newParameterNames <- traverse newName args
        let varsP = map VarP newParameterNames
        let fName = mkName name

        return ( LamE varsP $ functionApplicationE fName newParameterNames)
        -- return (LamE varsP  )

        where
            functionApplicationE:: Name -> [Name] -> Exp
            functionApplicationE functionName (n:ns)= foldr (\x acc -> AppE acc (VarE x) ) (AppE (VarE functionName) (VarE n)) ns
            functionApplicationE functionName [] = VarE functionName

type Range = (Int, Int)
class CsvTable t where
    getCols:: (Ord a) => [a] -> t a -> t a 
    getRows:: Range -> t a -> t a
    getTableContents:: t a -> VectorMatrix String

data ConcurrentCSV a = ConcurrentCSV {
    contents:: VectorMatrix String,
    headerMap:: M.Map a Int -- If we have Header in csv then we can map header to int 
} deriving (Eq, Show, Typeable)

createConcurrentCSV:: [[String]] -> ConcurrentCSV String
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