module Main where

import System.Environment (getArgs)
import System.IO

import Types
import Tokenizer
import Parser
import Interpreter
import Context

main :: IO ()
main = do
    -- Obtener los argumentos de la línea de comandos
    args <- getArgs
    putStrLn "PepeScript Interactive Interpreter (PPii) [Version 0.0.1]"
    case args of
        -- Verificar que se ha proporcionado al menos un argumento
        [filePath] -> do
            print "Processing file..."
            -- Leer el contenido del archivo
            content <- readFile filePath
            let (tokens, error) = tokenize content
            if tokens /= [] && error == None then do
                -- Parsing
                let res = parse tokens
                if Parser.is_success res then do
                    -- Interpreting
                    let (ast_rt, new_context) = visit_forest (get_ast_list res) (Context "<program>" NoParent [("true", Float 1), ("false", Float 0)])
                    print (ast_rt)
                    pepescript_cli new_context
                else do
                    -- Parse error
                    print "Parse error"
                    print res
            else do
                -- Tokenize error
                putStr "ERR "
                print error

        _ -> do
            putStrLn "\n\n\n\nExample of the fibonacci program in pepescript:\n\n"
            putStrLn "let n = 6;\nlet a = 0;\nlet fib = 1;\nlet c = 0;\n\nwhile n != 1 {\n\tlet c = a + fib;\n\tlet a = fib;\n\tlet fib = c;\n\tlet n = n - 1\n}\n\n"
            pepescript_cli (Context "<program>" NoParent [("true", Float 1), ("false", Float 0)])

pepescript_cli :: Context -> IO()
pepescript_cli context = do
    putStr "pepescript>> "
    hFlush stdout -- flush the buffer
    input <- getLine
    if input == "" then
        pepescript_cli context
    else do
        -- Tokenizing
        let (tokens, error) = tokenize input
        print tokens -- debug
        if tokens /= [] && error == None then do
            -- Parsing
            let res = parse tokens
            print (res)
            if Parser.is_success res then do
                -- Interpreting
                let (ast_rt, new_context) = visit_forest (get_ast_list res) context
                print (ast_rt)
                pepescript_cli new_context
            else
                -- Parse error
                print "Parse error"
        else do
            -- Tokenize error
            putStr "ERR "
            print error
    pepescript_cli context 