{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HaskelyzerTemplate.ASTToHaskell where
import Language.Haskell.TH 
import HaskelyzerAST.Lexer hiding (Name) 
import HaskelyzerCSV.Parser
import HaskelyzerTemplate.ConcurrentCSV
import System.Directory
import Language.Haskell.TH.Syntax

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