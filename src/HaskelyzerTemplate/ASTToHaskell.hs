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
import Language.Haskell.Interpreter (liftIO)
import Control.Monad (forM)
import Data.Maybe (isJust, fromJust)
import Data.Foldable (foldrM)
import Control.Concurrent.Async (Concurrently(runConcurrently, Concurrently), mapConcurrently)
import qualified Data.Map as Map

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
astExprToDec (Var n args haskFunctions ) = do
    let name = mkName n
    liftIO $ print haskFunctions

    -- generate uncapturable names which are not global
    argsAsVarP <- mapM (\x -> do y <- newName x; return (x, y)) args

    let argsMap = Map.fromList argsAsVarP

    haskFunctionsAsExp <- mapM (haskelyzerFunctionToExpr argsMap) haskFunctions
    let result = FunD name [Clause (map (VarP . snd) argsAsVarP) (NormalB $ composed haskFunctionsAsExp) []]
    liftIO $ print $ pprint result 
    return result
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

composeHaskelyzerFunction:: Map.Map String Name-> [HaskelyzerFunction] -> Q Exp
composeHaskelyzerFunction knownArgumentsMap [] = error "Can't compose empty list"
composeHaskelyzerFunction knownArgumentsMap (f:fs) = do
    hf <- haskelyzerFunctionToExpr knownArgumentsMap f
    foldrM helper hf fs

        where
            helper x acc =
                let f = haskelyzerFunctionToExpr knownArgumentsMap x in f >>= \a -> return $ UInfixE a (VarE '($)) acc

haskelyzerFunctionToExpr:: Map.Map String Name -> HaskelyzerFunction -> Q Exp
haskelyzerFunctionToExpr knownArgumentsMap (HaskelyzerFunction name args) = do
        -- Get variables from local parameters, if none exist use global ones
        createdArguments <-
                foldrM
                    (\arg acc -> 
                        let val = knownArgumentsMap Map.!? arg in 
                        if isJust val then return (fromJust val:acc) else 
                            return (mkName arg:acc)) 
                    [] 
                    args

        let varsP = map VarP createdArguments 
        let fName = mkName name

        return ( functionApplicationE fName createdArguments)
        -- return (LamE varsP  )

        where
            functionApplicationE:: Name -> [Name] -> Exp
            functionApplicationE functionName (n:ns)= foldr (\x acc -> AppE acc (VarE x) ) (AppE (VarE functionName) (VarE n)) ns
            functionApplicationE functionName [] = VarE functionName

haskelyzerFunctionToExpr knownArgumentsMap (Concurrent fs) = do

    -- Add function type check here (Should be IO a)
    -- ret <- mapM (\f -> let HaskelyzerFunction n _ = head f in lookupValueName n) fs

    -- let r = map (\x -> reify <$> x) ret  

    -- let t  = map fromJust $ filter isJust r 

    -- infos <- sequence t

    -- liftIO $ print infos

    -- End: Add type check here

    -- ret <- mapM (\f -> let HaskelyzerFunction n _ = head f in lookupValueName n) fs

    -- let r = map (\x -> reify <$> x) ret  

    -- let t  = map fromJust $ filter isJust r 

    composedFunctions <- mapM (composeHaskelyzerFunction knownArgumentsMap) fs
    let result = AppE (VarE 'runListConcurrently) $ ListE composedFunctions

    return result

isConcurrent:: HaskelyzerFunction -> Bool
isConcurrent (HaskelyzerFunction _ _) = False
isConcurrent (Concurrent _) = True


runListConcurrently:: [IO a] -> IO [a]
runListConcurrently = mapConcurrently id
