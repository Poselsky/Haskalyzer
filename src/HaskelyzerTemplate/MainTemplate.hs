{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
      Body(NormalB), Lit (StringL, IntegerL, RationalL), reify, location, Loc (loc_filename) )
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
import Language.Haskell.TH.Syntax (addDependentFile)
import System.FilePath
import System.Directory (makeAbsolute, doesFileExist)
import Data.Sequence (mapWithIndex)

type VectorMatrix a = V.Vector (V.Vector a)

class ToExp a where
    toExp:: a -> Exp

instance ToExp Double where
    toExp x = LitE $ RationalL $ toRational x

instance ToExp Integer where
    toExp x = LitE $ IntegerL x

instance ToExp String where
    toExp x = LitE $ StringL x

instance ToExp Literal where
  toExp (Int a) = toExp a
  toExp (String a) = toExp a
  toExp (Float a) = toExp a

generateSDL:: FilePath -> Q [Dec]
generateSDL filePath = do
    absoPathTkl <- runIO $ makeAbsolute filePath
    addDependentFile absoPathTkl -- Any time TKL file changes, we should recompile
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
    absoCsv <- runIO $ makeAbsolute path
    addDependentFile absoCsv

    csvFileContents <- runIO $ readFile path
    let either_csv = parseCSV path csvFileContents dataExpr

    case either_csv of
      Left pe -> fail $ "Can't parse csv: " ++ show pe
      Right (CSVFile contents _) -> do

        let tableName = mkName var
            csvDataTypeName = 'ConcurrentCSV
            headerMapName = 'headerMap  -- TODO: use reify to check ConcurrentCSV record type
            createName = 'createConcurrentCSV

        return $ FunD
            tableName
            [Clause
                []
                (NormalB $ AppE (VarE createName) (ListE $ map vectorBuilderHelper contents))
                []
            ]

    where
        vectorBuilderHelper:: [Literal] -> Exp
        vectorBuilderHelper literals = ListE $ map toLiteralWorkAround literals

        toLiteralWorkAround lit@(String str) = let toLitName = 'toStringLiteral in AppE (VarE toLitName) $ toExp lit
        toLiteralWorkAround lit@(Float f) = let toLitName = 'toFloatLiteral in AppE (VarE toLitName) $ toExp lit
        toLiteralWorkAround lit@(Int i) = let toLitName = 'toIntLiteral in AppE (VarE toLitName) $ toExp lit

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