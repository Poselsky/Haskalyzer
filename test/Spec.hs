import Parsing.Helpers (indentParser)
import HaskelyzerAST.Schema (schemaParser)
import Test.HUnit
import Data.Either (isRight, isLeft)
import HaskelyzerAST.Function (functionParser, variableParser)


main :: IO ()
main = do 
    runTestTT $ TestList $ [schemaTest] ++  map variableTest [variableTestInput, variableTestInput2, variableTestInput3]
    return ()


schemaTestInput :: String
schemaTestInput = 
    unlines [
        "{",
        "   let a = \"file.csv\" :",
        "       Int",
        "       String",
        "       Float",
        "}"
    ]


variableTestInput:: String
variableTestInput= "let a = doSomething x -> doSomethingElse"

variableTestInput2:: String
variableTestInput2 = "let a = doSomething a b c d e f g h i j k l m n o p q r s t u v w x y z"

variableTestInput3:: String
variableTestInput3 = "let a = doSomething"

schemaTest = TestCase ( do
        let parseResult = indentParser schemaParser schemaTestInput
        assertBool ("General schema parser: \n\t" ++ show parseResult) $ isRight parseResult
    )

variableTest testCaseInput = TestCase ( do
        let parseResult = indentParser variableParser testCaseInput 
        assertBool ("General schema parser: \n\t" ++ show parseResult) $ isRight parseResult
    )
