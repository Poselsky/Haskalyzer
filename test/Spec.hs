import Parsing.Helpers (indentParser)
import Haskelyzer.Schema (schemaParser)
import Test.HUnit
import Data.Either (isRight, isLeft)


main :: IO ()
main = do 
    runTestTT schemaTest
    return ()


schemaTestInput :: String
schemaTestInput = 
    unlines [
        "{",
        "let a = file.csv :",
        "   Int",
        "   String",
        "   Float",
        "}"
    ]

schemaTest = TestCase ( do
        let parseResult = indentParser schemaParser schemaTestInput
        assertBool ("General schema parser: \n\t" ++ show parseResult) $ isRight parseResult
    )