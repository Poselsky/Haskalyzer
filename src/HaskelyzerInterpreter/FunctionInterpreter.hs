module HaskelyzerInterpreter.FunctionInterpreter where
import HaskelyzerAST.Lexer (HaskelyzerFunction (HaskelyzerFunction))


functionStringBuilder:: HaskelyzerFunction -> String
functionStringBuilder (HaskelyzerFunction name args) = foldl (\x acc -> x ++ ' ':acc) name args

functionPipelineCompose:: [HaskelyzerFunction] -> String
functionPipelineCompose (f:fs) = foldl (\acc x -> wrap x ++ " . " ++ acc) (wrap f) fs
    where wrap x = "("++ (functionStringBuilder x) ++ ")"
functionPipelineCompose [] = "" 


