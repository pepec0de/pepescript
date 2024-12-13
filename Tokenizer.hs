module Tokenizer where

import Types

tokenize :: String -> ([Token], Error)
tokenize "" = ([], None)
tokenize (c:cs)
    | c == '+' = let (tokens, err) = tokenize cs in (TPlus : tokens, err)
    | c == '-' = let (tokens, err) = tokenize cs in (TMinus : tokens, err)
    | c == '*' = let (tokens, err) = tokenize cs in (TMult : tokens, err)
    | c == '/' = let (tokens, err) = tokenize cs in (TDiv : tokens, err)
    | c == '(' = let (tokens, err) = tokenize cs in (TLParen : tokens, err)
    | c == ')' = let (tokens, err) = tokenize cs in (TRParen : tokens, err)
    | c `elem` ['0'..'9'] = tokenizeNumber (c:cs)
    | c == ' ' = tokenize cs
    | otherwise = ([], IllegalCharError ("Invalid: " ++ [c]))

tokenizeNumber :: String -> ([Token], Error)
tokenizeNumber str = do
    let (num, rest) = span (`elem` (['0'..'9'] ++ ".")) str
    if '.' `elem` num then do
        let (tokens, err) = tokenize rest
        (TFloat ((read num) :: Float) : tokens, err)
    else do
        let (tokens, err) = tokenize rest
        (TInt (read num :: Int) : tokens, err)