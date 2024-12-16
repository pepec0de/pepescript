module Parser where

import Types

{-
Grammar

expr    : term ((PLUS | MINUS) term)* -> BinOpNode [+, -]
term    : factor ((MUL | DIV) factor)* -> BinOpNode [*, /]

factor  : (PLUS | MINUS) factor
        : power

power   : atom (POW factor)*

atom    : INT | FLOAT -> NumNode
        : LPAREN expr RPAREN
-}

-- Parser: Parses a list of tokens into an AST
parse :: [Token] -> ParseResult
parse tokens = do
    let (res, new_tokens) = parse_expr tokens
    if new_tokens /= [TEOF] then
        if is_success res then
            (Failure (InvalidSyntaxError "Must reach EOF"))
        else
            res
    else
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
build_expr_ast left (tok:tokens) = do
    if tok == TPlus || tok == TMinus then do
        let (res_right, new_tokens) = parse_term tokens
        if is_success res_right then
            build_expr_ast (BinOpNode tok left (get_ast res_right)) new_tokens
        else
            (res_right, [])
    else
        (Success left, (tok:tokens))

-- Parse a term (supports multiplication and division)
parse_term :: [Token] -> (ParseResult, [Token])
parse_term tokens = do
    let (res_left, new_tokens) = parse_factor tokens
    if is_success res_left then
        build_term_ast (get_ast res_left) new_tokens
    else
        (res_left, [])

build_term_ast :: AST -> [Token] -> (ParseResult, [Token])
build_term_ast left (tok:tokens) = do
    if tok == TMult || tok == TDiv then do
        let (res_right, new_tokens) = parse_factor tokens
        if is_success res_right then
            build_term_ast (BinOpNode tok left (get_ast res_right)) new_tokens
        else
            (res_right, [])
    else
        (Success left, (tok:tokens))

parse_power :: [Token] -> (ParseResult, [Token])
parse_power tokens = do
    let (res_left, new_tokens) = parse_atom tokens
    if is_success res_left then
        build_power_ast (get_ast res_left) new_tokens
    else
        (res_left, [])

build_power_ast :: AST -> [Token] -> (ParseResult, [Token])
build_power_ast left (TPow:tokens) = do
    let (res_right, new_tokens) = parse_atom tokens
    if is_success res_right then
        build_power_ast (BinOpNode TPow left (get_ast res_right)) new_tokens
    else
        (res_right, [])

build_power_ast left tokens = (Success left, tokens)

parse_factor :: [Token] -> (ParseResult, [Token])
parse_factor (TPlus:tokens) = do
    let (res, new_tokens) = parse_factor tokens
    if is_success res then
        (Success (UnaryOpNode TPlus (get_ast res)), new_tokens)
    else
        (res, new_tokens)

parse_factor (TMinus:tokens) = do
    let (res, new_tokens) = parse_factor tokens
    if is_success res then
        (Success (UnaryOpNode TMinus (get_ast res)), new_tokens)
    else
        (res, new_tokens)

parse_factor tokens = parse_power tokens

parse_atom :: [Token] -> (ParseResult, [Token])
parse_atom ((TInt n):tokens) = (Success (NumNode (TInt n)), tokens)
parse_atom ((TFloat n):tokens) = (Success (NumNode (TFloat n)), tokens)
parse_atom (TLParen:[]) = (Failure (InvalidSyntaxError "Expected a \')\'"), [])
parse_atom (TLParen:tokens) = do
    let (res, new_tokens) = parse_expr tokens
    if new_tokens == [] then
        (Failure (InvalidSyntaxError "Expected expression"), [])
    else
        if (new_tokens!!0) == TRParen then
            (Success (get_ast res), (tail new_tokens)) -- tail gives Warning TODO: fix it
        else
            (Failure (InvalidSyntaxError "Expected a \')\'"), [])

parse_atom (TRParen:[]) = (Failure (InvalidSyntaxError "Unexpected \')\'"), [])

parse_atom _ = (Failure (InvalidSyntaxError "Expected a '+', '-', '(' or number)"), [])

-- Helper function for ParseResult
is_success :: ParseResult -> Bool
is_success (Success _) = True
is_success _ = False

get_ast :: ParseResult -> AST
get_ast (Success ast) = ast