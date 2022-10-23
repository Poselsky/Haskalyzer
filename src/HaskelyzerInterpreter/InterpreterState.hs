module HaskelyzerInterpreter.InterpreterState where
import Control.Monad.RWS (RWST, execRWS, execRWST)
import HaskelyzerAST.Lexer
import Language.Haskell.Interpreter
import Data.Foldable (traverse_)
import HaskelyzerInterpreter.FunctionInterpreter (functionStringBuilder, functionPipelineCompose)


data InterpreterState =  InterpreterState {
    moduleText:: String
} deriving (Show)

type IOInterpreterRWS a = RWST () () InterpreterState IO a

beginInterpreter :: InterpreterState
beginInterpreter = InterpreterState {
    moduleText = ""
}

somethingSomething:: Expr -> String
somethingSomething (FunctionExpr f@(HaskelyzerFunction _ _)) = functionStringBuilder f
somethingSomething (Var name fs) = "let " ++ name ++ " = " ++ functionPipelineCompose fs
somethingSomething (Schema _ _) = ""
somethingSomething _ = error ""

something:: [Expr] -> IOInterpreterRWS ()
something ast = do
    runInterpreter $ do
        traverse_ (\x -> do
                let converted = somethingSomething x
                liftIO $ print converted
                -- eval converted
            ) ast
    return ()

runInterpreterRWS:: [Expr] -> IO ()
runInterpreterRWS ast = do

    execRWST (something ast) () beginInterpreter

    return ()
