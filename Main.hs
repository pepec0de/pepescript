module Main where

import Types
import Tokenizer
import Parser
import Interpreter
import System.IO

main :: IO()
main = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let (tokens, error) = tokenize input
    print tokens
    if tokens /= [] && error == None then do
        let res = parse tokens
        print (res)
        if Parser.is_success res then do
            let ast_rt = visit (get_ast res)
            print (ast_rt)
        else
            print "Parse error"
    else do
        putStr "ERR "
        print error
    main