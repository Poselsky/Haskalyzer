{-# LANGUAGE TemplateHaskell #-}
module Main where
import HaskelyzerTemplate.MainTemplate
import Control.Concurrent

-- a = 3
-- lel b = b + 1

sum:: Int
sum = 10

times:: IO () 
times = do 
    val <- myThreadId
    print val
    -- print "times"

plus:: IO () 
plus = print "plus"

vvv:: IO () 
vvv = do 
    print "vvv"

$(generateSDL "./testFiles/schema.tkl")

main :: IO ()
main = do
    kek
    -- putStrLn $ show a
    -- t <- readFile "./testFiles/schema.tkl"
    -- let ast = parseToplevelP t

    -- print ast

    -- case ast of Right e -> runInterpreterRWS e 
    --             Left e -> return () 
    -- print $ b 3
    return ()
