module Main where

import Types
import Tokenizer
import Parser
import Interpreter
import System.IO
import Context

main :: IO()
main = pepescript_cli (Context "<program>" NoParent [("null", Float 0)])

pepescript_cli :: Context -> IO()
pepescript_cli context = do
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
            let (ast_rt, new_context) = visit (get_ast res) context
            print (ast_rt)
            -- TODO : check runtime error (is printed anyway)
            pepescript_cli new_context
        else
            -- Parse error
            print "Parse error"
    else do
        -- Tokenize error
        putStr "ERR "
        print error
    pepescript_cli context 