module Interpreter where

import Types
import Context

visit :: AST -> Context -> (RuntimeResult, Context)
visit (NumNode tok) context = (RTSuccess (get_token_number tok), context)
visit (UnaryOpNode tok ast) context = do
    let (ast_rt, _) = visit ast context
    if is_success ast_rt then do
        let num = get_num ast_rt
        case tok of
            TPlus -> (RTSuccess num, context)
            TMinus -> (RTSuccess (mult (Float (-1)) num), context)
            _ -> (RTFailure (RuntimeError "Invalid operator token for UnaryOpNode"), context)
    else
        (ast_rt, context)

visit (BinOpNode tok left right) context = do
    let (left_rt, _) = visit left context
    let (right_rt, _) = visit right context
    if (is_success left_rt) && (is_success right_rt) then do
        let left = (get_num left_rt)
        let right = (get_num right_rt)
        case tok of
            TPlus -> (RTSuccess (add left right), context)
            TMinus -> (RTSuccess (sub left right), context)
            TMult -> (RTSuccess (mult left right), context)
            TDiv -> (RTSuccess (mDiv left right), context)
            TPow -> (RTSuccess (pow left right), context)
            _ -> (RTFailure (RuntimeError "Unrecognised token"), context)
    else
        (left_rt, context)

visit (VarAccessNode (TIdentifier identifier)) context = do
    let (found, value) = find_var identifier context
    if found then
        (RTSuccess value, context)
    else
        (RTFailure (RuntimeError ("'" ++ identifier ++ "' is not defined")), context)

visit (VarAssignNode (TIdentifier identifier) ast) context = do
    let (expression_rt, _) = visit ast context
    if is_success expression_rt then do
        let value = get_num expression_rt
        let new_context = add_var_to_context context identifier value
        (RTSuccess value, new_context)
    else
        (expression_rt, context)

visit _ context = (RTFailure (RuntimeError "Node implementation not implemented"), context)

get_token_number :: Token -> Number
get_token_number (TInt num) = Int num
get_token_number (TFloat num) = Float num

is_success :: RuntimeResult -> Bool
is_success (RTSuccess _) = True
is_success _ = False

get_num :: RuntimeResult -> Number
get_num (RTSuccess num) = num

add :: Number -> Number -> Number
add (Float a) (Float b) = Float (a+b)

sub :: Number -> Number -> Number
sub (Float a) (Float b) = Float (a-b)

mult :: Number -> Number -> Number
mult (Float a) (Float b) = Float (a*b)

mDiv :: Number -> Number -> Number
mDiv (Float a) (Float b) = Float (a/b)

pow :: Number -> Number -> Number
pow (Float a) (Float b) = Float (a**b)