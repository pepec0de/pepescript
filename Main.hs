module Main where

import Types
import Tokenizer
import Parser
import System.IO

main :: IO()
main = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let (tokens, error) = tokenize input
    if tokens /= [] && error == None then do
        let ast = parse tokens -- (ast, error)
        print ast
    else
        print error
    main