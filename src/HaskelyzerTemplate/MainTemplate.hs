{-# LANGUAGE QuasiQuotes #-}
module HaskelyzerTemplate.MainTemplate where
import Language.Haskell.TH
    ( mkName,
      Quote(newName),
      Exp(LamE, VarE, AppE, InfixE),
      Q,
      Dec,
      runIO,
      Pat(VarP),
      Clause(Clause),
      Name,
      Dec(FunD),
      Body(NormalB) )
import HaskelyzerAST.Parser
import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering))
import GHC.IO.Handle.FD (stdout, stderr)
import HaskelyzerAST.Lexer hiding (Name)
import Control.Monad ((>=>))
import qualified GHC.Base
import HaskelyzerCSV.Parser (parseCSV)

generateSDL:: FilePath -> Q [Dec]
generateSDL filePath = do
    runIO $ hSetBuffering stdout NoBuffering -- I want prints during compilation
    runIO $ hSetBuffering stderr NoBuffering -- I want prints during compilation

    t <- runIO $ readFile filePath
    let ast = parseToplevelP t

    runIO $ print ast
    decs <- case ast of
      Right exs -> mapM astExprToDec exs
      Left pe -> error $ show pe

    -- error ""
    return decs

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
    let name = mkName var 

    csvFileContents <- runIO $ readFile path
    either_csv <- runIO $ parseCSV path csvFileContents
    return $ FunD name []

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


data ConcurrentCSV = ConcurrentCSV {
    splitRowBy:: Int,
    schema:: Schema,
    columnNames:: Maybe [String] -- CSV can have 
} 
