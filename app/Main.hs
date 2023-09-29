{-# LANGUAGE TemplateHaskell #-}
module Main where
import HaskelyzerTemplate.MainTemplate
import Control.Concurrent

-- a = 3
-- lel b = b + 1

sum:: Int
sum = 10

times:: Int -> IO () 
times a = do 
    val <- myThreadId
    print val
    -- print "times"

plus:: Int -> Int
plus a = a + 4

minus:: Int -> IO ()
minus v = do 
    print v

vvv:: Int -> IO () 
vvv _ = do 
    print "ok"

$(generateHaskalyzer "./testFiles/schema.tkl")

main :: IO ()
main = do
    kek 4
    -- putStrLn $ show a
    -- t <- readFile "./testFiles/schema.tkl"
    -- let ast = parseToplevelP t

    -- print ast

    -- case ast of Right e -> runInterpreterRWS e 
    --             Left e -> return () 
    -- print $ b 3
    return ()
