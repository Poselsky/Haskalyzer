module HaskelyzerTemplate.MainTemplate where
import Language.Haskell.TH
import System.Directory
import Language.Haskell.TH.Syntax
import System.IO
import HaskelyzerAST.Parser
import HaskelyzerTemplate.ConcurrentCSV
import HaskelyzerTemplate.ASTToHaskell

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


