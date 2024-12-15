module Parser where

import Types

{-
Grammar

expr    : term ((PLUS | MINUS) term)* -> BinOpNode [+, -]
term    : factor ((MUL | DIV) factor)* -> BinOpNode [*, /]

factor  : INT | FLOAT -> NumNode

-}
-- Parser: Parses a list of tokens into an AST
parse :: [Token] -> ParseResult
parse tokens = do
    let (res, []) = parse_expr tokens
    if is_success res then
        res
    else
        res
        
parse_expr :: [Token] -> (ParseResult, [Token])
parse_expr tokens = do
    let (res_left, new_tokens) = parse_term tokens
    if is_success res_left then
        (build_expr_ast (get_ast res_left) new_tokens)
    else
        (res_left, [])

build_expr_ast :: AST -> [Token] -> (ParseResult, [Token])
build_expr_ast left (TPlus:tokens) = do
    let (res_right, new_tokens) = parse_term tokens
    if is_success res_right then
        build_expr_ast (BinOpNode left TPlus (get_ast res_right)) new_tokens
    else
        (res_right, [])

build_expr_ast left (TMinus:tokens) = do
    let (res_right, new_tokens) = parse_term tokens
    if is_success res_right then
        build_expr_ast (BinOpNode left TMinus (get_ast res_right)) new_tokens
    else
        (res_right, [])

build_expr_ast left tokens = (Success left, tokens)

-- Parse a term (supports multiplication and division)
parse_term :: [Token] -> (ParseResult, [Token])
parse_term tokens = do
    let (res_left, new_tokens) = parse_factor tokens
    if is_success res_left then
        build_term_ast (get_ast res_left) new_tokens
    else
        (res_left, [])

build_term_ast :: AST -> [Token] -> (ParseResult, [Token])
build_term_ast left (TMult:tokens) = do
    let (res_right, new_tokens) = parse_factor tokens
    if is_success res_right then
        build_term_ast (BinOpNode left TMult (get_ast res_right)) new_tokens
    else
        (res_right, [])

build_term_ast left (TDiv:tokens) = do
    let (res_right, new_tokens) = parse_factor tokens
    if is_success res_right then
        build_term_ast (BinOpNode left TDiv (get_ast res_right)) new_tokens
    else
        (res_right, [])
build_term_ast left tokens = (Success left, tokens)

-- Parse a factor (supports integers)
parse_factor :: [Token] -> (ParseResult, [Token])
parse_factor ((TInt n):tokens) = (Success (NumNode (TInt n)), tokens)
parse_factor ((TFloat n):tokens) = (Success (NumNode (TFloat n)), tokens)
parse_factor _ = (Failure (InvalidSyntaxError "Expected a number"), [])

is_success :: ParseResult -> Bool
is_success (Success _) = True
is_success _ = False

get_ast :: ParseResult -> AST
get_ast (Success ast) = ast