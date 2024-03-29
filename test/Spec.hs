import Parsing.Helpers (indentParser)
import HaskelyzerAST.Schema (schemaParser)
import Test.HUnit
import Data.Either (isRight, isLeft)
import HaskelyzerAST.Function (functionParser, variableParser)


main :: IO ()
main = do 
    runTestTT $ TestList $ map schemaTest [schemaTestInput, schemaTestInput2] 
        ++  map variableTest [variableTestInput, variableTestInput2, variableTestInput3, variableTestInput4]
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

schemaTestInput2 :: String
schemaTestInput2 = 
    unlines [
        "{",
        "   let a = \"file.csv\" :",
        "       (a,Int)",
        "       (b,String)",
        "       (c,Float)",
        "}"
    ]

variableTestInput:: String
variableTestInput= "let a = doSomething x -> doSomethingElse"

variableTestInput2:: String
variableTestInput2 = "let a = doSomething a b c d e f g h i j k l m n o p q r s t u v w x y z"

variableTestInput3:: String
variableTestInput3 = "let a = doSomething"

variableTestInput4:: String
variableTestInput4 = "let a = doSomething | sum a b"

schemaTest testCaseInput = TestCase ( do
        let parseResult = indentParser schemaParser testCaseInput 
        assertBool ("General schema parser: \n\t" ++ show parseResult) $ isRight parseResult
    )

variableTest testCaseInput = TestCase ( do
        let parseResult = indentParser variableParser testCaseInput 
        assertBool ("General schema parser: \n\t" ++ show parseResult) $ isRight parseResult
    )
