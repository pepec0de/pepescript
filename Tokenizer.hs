module Tokenizer where

import Types

tokenize :: String -> ([Token], Error)
tokenize "" = ([TEOF], None)
tokenize (c:cs)
    | c == ' ' = tokenize cs
    | c == '\n' = tokenize cs
    | c == '\t' = tokenize cs
    | c == '+' = let (tokens, err) = tokenize cs in (TPlus : tokens, err)
    | c == '-' = let (tokens, err) = tokenize cs in (TMinus : tokens, err)
    | c == '*' = let (tokens, err) = tokenize cs in (TMult : tokens, err)
    | c == '/' = let (tokens, err) = tokenize cs in (TDiv : tokens, err)
    | c == '^' = let (tokens, err) = tokenize cs in (TPow : tokens, err)
    | c == '(' = let (tokens, err) = tokenize cs in (TLParen : tokens, err)
    | c == ')' = let (tokens, err) = tokenize cs in (TRParen : tokens, err)
    | c `elem` (['a'..'z'] ++ ['A'..'Z']) = tokenizeIdentifier (c:cs)
    | c `elem` ['0'..'9'] = tokenizeNumber (c:cs)
    | c == '=' = tokenizeEq cs
    | c == '!' = tokenizeNotEq cs
    | c == '<' = tokenizeLess cs
    | c == '>' = tokenizeGreat cs
    | c == '&' = tokenizeAnd cs
    | c == '|' = tokenizeOr cs
    | otherwise = ([], IllegalCharError ("Invalid: " ++ [c]))

tokenizeIdentifier :: String -> ([Token], Error)
tokenizeIdentifier str = do
    let (identifier, rest) = span (`elem` (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "_")) str
    let token = get_keyword_or_identifier identifier
    let (tokens, err) = tokenize rest in (token: tokens, err)

get_keyword_or_identifier :: String -> Token
get_keyword_or_identifier str = 
    case str of
        "let" -> TKeyword_let
        "if" -> TKeyword_if
        "while" -> TKeyword_while
        _ -> TIdentifier str

tokenizeNumber :: String -> ([Token], Error)
tokenizeNumber str = do
    let (num, rest) = span (`elem` (['0'..'9'] ++ ".")) str
    if '.' `elem` num then do
        let (tokens, err) = tokenize rest
        (TFloat ((read num) :: Float) : tokens, err)
    else do
        let (tokens, err) = tokenize rest
        (TFloat (read num :: Float) : tokens, err)

tokenizeEq :: String -> ([Token], Error)
tokenizeEq (c:cs) =
    if c == '=' then
        let (tokens, err) = tokenize cs in (TEqEq : tokens, err)
    else
        let (tokens, err) = tokenize (c:cs) in (TEq : tokens, err)

tokenizeNotEq :: String -> ([Token], Error)
tokenizeNotEq (c:cs) =
    if c == '=' then
        let (tokens, err) = tokenize cs in (TNotEq : tokens, err)
    else
        let (tokens, err) = tokenize (c:cs) in (TNot : tokens, err)

tokenizeLess :: String -> ([Token], Error)
tokenizeLess (c:cs) =
    if c == '=' then
        let (tokens, err) = tokenize cs in (TLtEq : tokens, err)
    else
        let (tokens, err) = tokenize (c:cs) in (TLt : tokens, err)

tokenizeGreat :: String -> ([Token], Error)
tokenizeGreat (c:cs) =
    if c == '=' then
        let (tokens, err) = tokenize cs in (TGtEq : tokens, err)
    else
        let (tokens, err) = tokenize (c:cs) in (TGt : tokens, err)

tokenizeAnd :: String -> ([Token], Error)
tokenizeAnd ('&':cs) = let (tokens, err) = tokenize cs in (TAnd : tokens, err)
tokenizeAnd (c:_) = ([], IllegalCharError ("Not supported operator: &" ++ [c]))

tokenizeOr :: String -> ([Token], Error)
tokenizeOr ('|':cs) = let (tokens, err) = tokenize cs in (TOr : tokens, err)
tokenizeOr (c:_) = ([], IllegalCharError ("Not supported operator: |" ++ [c]))
