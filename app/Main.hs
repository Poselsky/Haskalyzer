{-# LANGUAGE TemplateHaskell #-}
module Main where
import HaskelyzerTemplate.MainTemplate

a = 3
lel b = b + 1

$(generateSDL "./testFiles/schema.tkl")

main :: IO ()
main = do
    -- t <- readFile "./testFiles/schema.tkl"
    -- let ast = parseToplevelP t

    -- print ast

    -- case ast of Right e -> runInterpreterRWS e 
    --             Left e -> return () 
    print $ b 3
    return ()
