module NewParser where

import Types

-- Parser: Parses a list of tokens into an AST
parse :: [Token] -> ParseResult
parse tokens = do
    let (res, new_tokens) = parse_expr tokens
    if new_tokens /= [TEOF] then
        if is_success res then
            (ParseFailure (InvalidSyntaxError "Must reach EOF"))
        else
            res
    else
        if is_success res then
            res
        else
            res

parse_expr :: [Token] -> (ParseResult, [Token])
parse_expr tokens = bin_op parse_term [TPlus, TMinus] parse_term tokens

parse_term :: [Token] -> (ParseResult, [Token])
parse_term tokens = bin_op parse_factor [TMult, TDiv] parse_factor tokens

parse_factor :: [Token] -> (ParseResult, [Token])
parse_factor (TPlus:tokens) = do
    let (res, new_tokens) = parse_factor tokens
    if is_success res then
        (ParseSuccess (UnaryOpNode TPlus (get_ast res)), new_tokens)
    else
        (res, [])

parse_factor (TMinus:tokens) = do
    let (res, new_tokens) = parse_factor tokens
    if is_success res then
        (ParseSuccess (UnaryOpNode TMinus (get_ast res)), new_tokens)
    else
        (res, [])

parse_factor ((TFloat n):tokens) = (ParseSuccess (NumNode (TFloat n)), tokens)
parse_factor _ = (ParseFailure (InvalidSyntaxError "Expected a '+', '-', '(', number, 'let' or identifier)"), [])

bin_op :: ([Token] -> (ParseResult, [Token])) -> [Token] -> ([Token] -> (ParseResult, [Token])) -> [Token] -> (ParseResult, [Token])
bin_op func_left ops func_right tokens = do
    -- Parse the left-hand side
    let (left_res, left_tokens) = func_left tokens
    if not $ is_success left_res then
        (left_res, [])
    else
        parseRest (get_ast left_res) left_tokens
    where 
        parseRest left (tok:left_tokens)
            | tok `elem` ops = do
                let (right_res, right_tokens) = func_right left_tokens
                if not $ is_success right_res then
                    (right_res, [])
                else
                    parseRest (BinOpNode tok left (get_ast right_res)) right_tokens
            | otherwise = (ParseSuccess left, (tok:left_tokens))

-- Helper function for ParseResult
is_success :: ParseResult -> Bool
is_success (ParseSuccess _) = True
is_success _ = False

get_ast :: ParseResult -> AST
get_ast (ParseSuccess ast) = ast