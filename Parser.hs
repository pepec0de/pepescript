module Parser where

import Types

{-
Grammar

expr    : TKeyword:let IDENTIFIER TEq expr 
          term ((PLUS | MINUS) term)* -> BinOpNode [+, -]

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
            (ParseFailure (InvalidSyntaxError "Must reach EOF"))
        else
            res
    else
        if is_success res then
            res
        else
            res

is_identifier :: Token -> Bool
is_identifier (TIdentifier _) = True
is_identifier _ = False

-- METHOD FOR BUILDING THE LET AST
build_let_expr :: [Token] -> (ParseResult, [Token])
build_let_expr (tok_identifier:tok_equals:tokens)
    | is_identifier tok_identifier && tok_equals == TEq = do
        let (expression, rest) = parse_expr tokens
        if is_success expression then
            (ParseSuccess (VarAssignNode tok_identifier (get_ast expression)), rest)
        else
            (expression, rest)
    | otherwise = (ParseFailure (InvalidSyntaxError "Let clause must be: let IDENTIFIER = EXPRESSION"), [])
        
parse_expr :: [Token] -> (ParseResult, [Token])
parse_expr (TKeyword_let:tokens) = build_let_expr tokens

parse_expr tokens = do
    let (res_left, new_tokens) = parse_term tokens
    if is_success res_left then
        (build_expr_ast (get_ast res_left) new_tokens)
    else
        (res_left, [])

build_expr_ast :: AST -> [Token] -> (ParseResult, [Token])
build_expr_ast left (tok:tokens)
    | tok == TPlus || tok == TMinus = do
        let (res_right, new_tokens) = parse_term tokens
        if is_success res_right then
            build_expr_ast (BinOpNode tok left (get_ast res_right)) new_tokens
        else
            (res_right, [])
    | otherwise = (ParseSuccess left, (tok:tokens))

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
        (ParseSuccess left, (tok:tokens))

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

build_power_ast left tokens = (ParseSuccess left, tokens)

parse_factor :: [Token] -> (ParseResult, [Token])
parse_factor (TPlus:tokens) = do
    let (res, new_tokens) = parse_factor tokens
    if is_success res then
        (ParseSuccess (UnaryOpNode TPlus (get_ast res)), new_tokens)
    else
        (res, new_tokens)

parse_factor (TMinus:tokens) = do
    let (res, new_tokens) = parse_factor tokens
    if is_success res then
        (ParseSuccess (UnaryOpNode TMinus (get_ast res)), new_tokens)
    else
        (res, new_tokens)

parse_factor tokens = parse_power tokens

parse_atom :: [Token] -> (ParseResult, [Token])
parse_atom ((TInt n):tokens) = (ParseSuccess (NumNode (TInt n)), tokens)
parse_atom ((TFloat n):tokens) = (ParseSuccess (NumNode (TFloat n)), tokens)
parse_atom ((TIdentifier identifier):tokens) = (ParseSuccess (VarAccessNode (TIdentifier identifier)), tokens)
parse_atom (TLParen:[]) = (ParseFailure (InvalidSyntaxError "Expected a \')\'"), [])
parse_atom (TLParen:tokens) = do
    let (res, new_tokens) = parse_expr tokens
    if new_tokens == [] then
        (ParseFailure (InvalidSyntaxError "Expected expression"), [])
    else
        if (new_tokens!!0) == TRParen then
            (ParseSuccess (get_ast res), (tail new_tokens)) -- tail gives Warning TODO: fix it
        else
            (ParseFailure (InvalidSyntaxError "Expected a \')\'"), [])

parse_atom (TRParen:[]) = (ParseFailure (InvalidSyntaxError "Unexpected \')\'"), [])

parse_atom _ = (ParseFailure (InvalidSyntaxError "Expected a '+', '-', '(', number, 'let' or identifier)"), [])

-- Helper function for ParseResult
is_success :: ParseResult -> Bool
is_success (ParseSuccess _) = True
is_success _ = False

get_ast :: ParseResult -> AST
get_ast (ParseSuccess ast) = ast