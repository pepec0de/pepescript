module Parser where

import Types

{-
Grammar

expr    : term ((PLUS | MINUS) term)* -> BinOpNode [+, -]
term    : factor ((MUL | DIV) factor)* -> BinOpNode [*, /]

factor  : INT | FLOAT -> NumNode

-}
-- Parser: Parses a list of tokens into an AST
parse :: [Token] -> (AST, Error)
parse tokens = do
    let (ast, [], err) = parse_expr tokens
    (ast, err)

parse_expr :: [Token] -> (AST, [Token], Error)
parse_expr tokens = do
    let (left, new_tokens, err) = parse_term tokens
    (build_expr_ast left new_tokens, err)

build_expr_ast :: AST -> [Token] -> (AST, [Token])
build_expr_ast left (TPlus:tokens) = do
    let (right, new_tokens) = parse_term tokens
    build_expr_ast (BinOpNode left TPlus right) new_tokens

build_expr_ast left (TMinus:tokens) = do
    let (right, new_tokens) = parse_term tokens
    build_expr_ast (BinOpNode left TMinus right) new_tokens
build_expr_ast left tokens = (left, tokens)

-- Parse a term (supports multiplication and division)
parse_term :: [Token] -> (AST, [Token], Error)
parse_term tokens = do
    let (left, new_tokens) = parse_factor tokens
    build_term_ast left new_tokens

build_term_ast :: AST -> [Token] -> (AST, [Token], Error)
build_term_ast left (TMult:tokens) = do
    let (right, new_tokens, err) = parse_factor tokens
    build_term_ast (BinOpNode left TMult right) new_tokens
build_term_ast left (TDiv:tokens) = do
    let (right, new_tokens, err) = parse_factor tokens
    build_term_ast (BinOpNode left TDiv right) new_tokens
build_term_ast left tokens = (left, tokens, None)

-- Parse a factor (supports integers)
parse_factor :: [Token] -> (AST, [Token], Error)
parse_factor ((TInt n):tokens) = (NumNode (TInt n), tokens, None)
parse_factor ((TFloat n):tokens) = (NumNode (TFloat n), tokens, None)
parse_factor _ = (Empty, [], IllegalSyntaxError "Expected a number")


