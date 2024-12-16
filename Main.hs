module Main where

import Types
import Tokenizer
import Parser
import Interpreter
import System.IO

main :: IO()
main = do
    putStr "> "
    hFlush stdout -- flush the buffer
    input <- getLine

    -- Tokenizing
    let (tokens, error) = tokenize input
    print tokens -- debug
    if tokens /= [] && error == None then do
        -- Parsing
        let res = parse tokens
        print (res)
        if Parser.is_success res then do
            -- Interpreting
            let context = Context "<program>" NoParent
            let ast_rt = visit (get_ast res) context
            print (ast_rt)
            -- TODO : check runtime error
        else
            -- Parse error
            print "Parse error"
    else do
        -- Tokenize error
        putStr "ERR "
        print error
    main