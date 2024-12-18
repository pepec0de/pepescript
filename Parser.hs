module Parser where

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
parse_expr (TKeyword_let:tokens) = build_let_expr tokens
parse_expr tokens = do
    let (res_left, new_tokens) = parse_comp_expr tokens
    if is_success res_left then
        (build_expr_ast (get_ast res_left) new_tokens)
    else
        (res_left, [])

-- METHOD FOR BUILDING THE LET AST
build_let_expr :: [Token] -> (ParseResult, [Token])
build_let_expr (tok_identifier:tok_equals:tokens)
    | is_identifier tok_identifier && tok_equals == TEq = do
        let (expression, rest) = parse_expr tokens
        if is_success expression then
            (ParseSuccess (VarAssignNode tok_identifier (get_ast expression)), rest)
        else
            (expression, [])
    | otherwise = (ParseFailure (InvalidSyntaxError "Let clause must be: let IDENTIFIER = EXPRESSION"), [])

build_expr_ast :: AST -> [Token] -> (ParseResult, [Token])
build_expr_ast left (tok:tokens)
    | tok == TAnd || tok == TOr = do
        let (res_right, new_tokens) = parse_comp_expr tokens
        if is_success res_right then
            (build_expr_ast (BinOpNode tok left (get_ast res_right)) new_tokens)
        else
            (res_right, [])
    | otherwise = (ParseSuccess left, (tok:tokens))

parse_comp_expr :: [Token] -> (ParseResult, [Token])
parse_comp_expr (tok:tokens)
    | tok == TNot = do
        let (res, new_tokens) = parse_comp_expr tokens
        if is_success res then
            (ParseSuccess (UnaryOpNode TNot (get_ast res)), new_tokens)
        else
            (res, [])

    | otherwise = do
        let (res, new_tokens) = parse_arith_expr (tok:tokens)
        if is_success res then
            (build_comp_expr (get_ast res) new_tokens)
        else
            (res, [])

build_comp_expr :: AST -> [Token] -> (ParseResult, [Token])
build_comp_expr left (tok:tokens)
    | tok `elem` [TEqEq, TNotEq, TLt, TLtEq, TGt, TGtEq] = do
        let (res_right, new_tokens) = parse_arith_expr tokens
        if is_success res_right then
            build_comp_expr (BinOpNode tok left (get_ast res_right)) new_tokens
        else
            (res_right, [])
    | otherwise = (ParseSuccess left, (tok:tokens))

parse_arith_expr :: [Token] -> (ParseResult, [Token])
parse_arith_expr tokens = do
    let (res_left, new_tokens) = parse_term tokens
    if is_success res_left then
        (build_arith_expr_ast (get_ast res_left) new_tokens)
    else
        (res_left, [])

build_arith_expr_ast :: AST -> [Token] -> (ParseResult, [Token])
build_arith_expr_ast left (tok:tokens)
    | tok == TPlus || tok == TMinus = do
        let (res_right, new_tokens) = parse_term tokens
        if is_success res_right then
            build_arith_expr_ast (BinOpNode tok left (get_ast res_right)) new_tokens
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
    let (res_left, new_tokens) = parse_call tokens
    if is_success res_left then
        build_power_ast (get_ast res_left) new_tokens
    else
        (res_left, [])

build_power_ast :: AST -> [Token] -> (ParseResult, [Token])
build_power_ast left (TPow:tokens) = do
    let (res_right, new_tokens) = parse_call tokens
    if is_success res_right then
        build_power_ast (BinOpNode TPow left (get_ast res_right)) new_tokens
    else
        (res_right, [])
build_power_ast left tokens = (ParseSuccess left, tokens)

parse_call :: [Token] -> (ParseResult, [Token])
parse_call tokens = do
    let (res_left, new_tokens) = parse_atom tokens
    if is_success res_left then
        build_call_ast (get_ast res_left) new_tokens
    else
        (res_left, [])

build_call_ast :: AST -> [Token] -> (ParseResult, [Token])
build_call_ast tokens = do

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
parse_atom (TKeyword_if:tokens) = parse_if_expr tokens
parse_atom (TKeyword_while:tokens) = parse_while_expr tokens
parse_atom _ = (ParseFailure (InvalidSyntaxError "Expected a '+', '-', '(', number, 'let' or identifier)"), [])

parse_if_expr :: [Token] -> (ParseResult, [Token])
parse_if_expr tokens = do -- TODO: understand why this works
    let (condition_res, expression_tokens) = parse_expr tokens
    if expression_tokens!!0 == TKeyword_then then -- check the 'then' keyword
        if is_success condition_res then do
            -- Parse expression after then
            let (expression_res, else_tokens) = parse_expr (drop 1 expression_tokens) -- drop the 'then' keyword
            if is_success expression_res then do
                if else_tokens!!0 == TKeyword_else then do
                    let (else_res, new_tokens) = parse_else_expr (drop 1 else_tokens)
                    if is_success else_res then
                        (ParseSuccess (IfNode ((get_ast condition_res), (get_ast expression_res)) (get_ast else_res)), new_tokens)
                    else
                        (else_res, [])
                else
                    (ParseSuccess (IfNode ((get_ast condition_res), (get_ast expression_res)) Empty), else_tokens)
            else
                (expression_res, [])
        else
            (condition_res, [])
    else
        (ParseFailure (InvalidSyntaxError "Expected keyword 'then' after expression of if clause"), [])

parse_else_expr :: [Token] -> (ParseResult, [Token])
parse_else_expr tokens = do
    let (else_res, new_tokens) = parse_expr tokens
    if is_success else_res then
        (else_res, new_tokens)
    else
        (else_res, [])

parse_while_expr :: [Token] -> (ParseResult, [Token])
parse_while_expr tokens = do
    let (condition_res, expression_tokens) = parse_expr tokens
    if expression_tokens!!0 == TKeyword_then then -- check the 'then' keyword
        if is_success condition_res then do
            -- Parse expression after then
            let (expression_res, new_tokens) = parse_expr (drop 1 expression_tokens) -- drop the 'then' keyword
            if is_success expression_res then do
                (ParseSuccess (WhileNode (get_ast condition_res) (get_ast expression_res)), new_tokens)
            else
                (expression_res, [])
        else
            (condition_res, [])
    else
        (ParseFailure (InvalidSyntaxError "Expected keyword 'then' after expression of while clause"), [])

-- Helper function for ParseResult
is_success :: ParseResult -> Bool
is_success (ParseSuccess _) = True
is_success _ = False

get_ast :: ParseResult -> AST
get_ast (ParseSuccess ast) = ast