module Parser where

import Types

-- Parser: Parses a list of tokens into an AST
parse :: [Token] -> ParseResult
parse tokens = do
    let (res, new_tokens) = parse_statements tokens
    if not $ is_success res then
        res
    else
        if new_tokens /= [TEOF] then
            (ParseFailure (InvalidSyntaxError "Unrecognisable error"))
        else
            res

-- General function
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
            | otherwise = (ParseSuccess [left], (tok:left_tokens))

parse_statements :: [Token] -> (ParseResult, [Token])
parse_statements tokens = do
    let (res, new_tokens) = parse_expr tokens
    if not $ is_success res then
        (res, new_tokens)
    else
        case new_tokens of
            (TSemicolon:rest) -> do
                let (next_res, final_tokens) = parse_statements rest
                if not $ is_success next_res then
                    (next_res, final_tokens)
                else
                    (ParseSuccess ((get_ast_list res) ++ (get_ast_list next_res)), final_tokens)
            (TRBracket:rest) -> (res, rest)
            (TEOF:[]) -> (res, new_tokens)
            _ ->
                (res, new_tokens)

parse_expr :: [Token] -> (ParseResult, [Token])
parse_expr (TKeyword_let:tok_identifier:tok_equals:tokens) -- BUILD LET clause
    | is_identifier tok_identifier && tok_equals == TEq = do
        let (expression, rest) = parse_expr tokens
        if is_success expression then
            (ParseSuccess [(VarAssignNode tok_identifier (get_ast expression))], rest)
        else
            (expression, [])
    | otherwise = (ParseFailure (InvalidSyntaxError "Let clause must be: let IDENTIFIER = EXPRESSION"), [])
parse_expr tokens = bin_op parse_comp_expr [TAnd, TOr] parse_comp_expr tokens

parse_comp_expr :: [Token] -> (ParseResult, [Token])
parse_comp_expr (tok:tokens)
    | tok == TNot = do
        let (res, new_tokens) = parse_comp_expr tokens
        if is_success res then
            (ParseSuccess [(UnaryOpNode TNot (get_ast res))], new_tokens)
        else
            (res, [])
    | otherwise = bin_op parse_arith_expr [TEqEq, TNotEq, TLt, TLtEq, TGt, TGtEq] parse_arith_expr (tok:tokens)

parse_arith_expr :: [Token] -> (ParseResult, [Token])
parse_arith_expr tokens = bin_op parse_term [TPlus, TMinus] parse_term tokens

-- Parse a term (supports multiplication and division)
parse_term :: [Token] -> (ParseResult, [Token])
parse_term tokens = bin_op parse_factor [TMult, TDiv] parse_factor tokens

parse_power :: [Token] -> (ParseResult, [Token])
parse_power tokens = bin_op parse_call [TPow] parse_factor tokens

parse_call :: [Token] -> (ParseResult, [Token])
parse_call tokens = do
    let (res_left, new_tokens) = parse_atom tokens
    if not $ is_success res_left then
        (res_left, [])
    else
        if new_tokens!!0 == TLParen then do
            -- Grab CallNode
            let new_tokens2 = drop 1 new_tokens
            if new_tokens2!!0 == TRParen then
                (ParseSuccess [(CallFuncNode (get_ast res_left) [])], drop 1 new_tokens2)
            else
                (ParseFailure (InvalidSyntaxError "Function arguments are not supported yet"), [])
        else
            -- Return atom
            (res_left, new_tokens)

parse_factor :: [Token] -> (ParseResult, [Token])
parse_factor (tok:tokens)
    | tok `elem` [TPlus, TMinus] = do
        let (res, new_tokens) = parse_factor tokens
        if is_success res then
            (ParseSuccess [(UnaryOpNode tok (get_ast res))], new_tokens)
        else
            (res, [])
    | otherwise = parse_power (tok:tokens)

parse_atom :: [Token] -> (ParseResult, [Token])
parse_atom ((TInt n):tokens) = (ParseSuccess [(NumNode (TInt n))], tokens)
parse_atom ((TFloat n):tokens) = (ParseSuccess [(NumNode (TFloat n))], tokens)
parse_atom ((TIdentifier identifier):tokens) = (ParseSuccess [(VarAccessNode (TIdentifier identifier))], tokens)
parse_atom (TLParen:[]) = (ParseFailure (InvalidSyntaxError "Expected a \')\'"), [])
parse_atom (TLParen:tokens) = do
    let (res, new_tokens) = parse_expr tokens
    if new_tokens == [] then
        (ParseFailure (InvalidSyntaxError "Expected expression"), [])
    else
        if (new_tokens!!0) == TRParen then
            (ParseSuccess [(get_ast res)], (drop 1 new_tokens))
        else
            (ParseFailure (InvalidSyntaxError "Expected a \')\'"), [])

parse_atom (TRParen:[]) = (ParseFailure (InvalidSyntaxError "Unexpected \')\'"), [])
parse_atom (TKeyword_if:tokens) = parse_if_expr tokens
parse_atom (TKeyword_while:tokens) = parse_while_expr tokens
parse_atom _ = (ParseFailure (InvalidSyntaxError "Expected a '+', '-', '(', number, 'let' or identifier)"), [])

parse_if_expr :: [Token] -> (ParseResult, [Token])
parse_if_expr tokens = do -- TODO: understand why this works
    let (condition_res, expression_tokens) = parse_expr tokens
    if expression_tokens!!0 == TLBracket then -- check the 'then' keyword
        if is_success condition_res then do
            -- Parse expression after then
            let (expression_res, else_tokens) = parse_statements (drop 1 expression_tokens) -- drop the 'then' keyword
            if is_success expression_res then do
                if else_tokens!!0 == TKeyword_else then do
                    let (else_res, new_tokens) = parse_else_expr (drop 1 else_tokens)
                    if is_success else_res then
                        (ParseSuccess [(IfNode ((get_ast condition_res), (get_ast_list expression_res)) (get_ast_list else_res))], new_tokens)
                    else
                        (else_res, [])
                else
                    (ParseSuccess [(IfNode ((get_ast condition_res), (get_ast_list expression_res)) [])], else_tokens)
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
    if expression_tokens!!0 == TLBracket then -- check the 'then' keyword
        if is_success condition_res then do
            -- Parse expression after then
            let (expression_res, new_tokens) = parse_expr (drop 1 expression_tokens) -- drop the 'then' keyword
            if is_success expression_res then do
                (ParseSuccess [(WhileNode (get_ast condition_res) (get_ast_list expression_res))], new_tokens)
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

get_ast_list :: ParseResult -> [AST]
get_ast_list (ParseSuccess ast) = ast

get_ast :: ParseResult -> AST
get_ast (ParseSuccess ast) = (ast!!0)