module Interpreter where

import Types

visit :: AST -> RuntimeResult
visit (NumNode tok) = (RTSuccess (get_token_number tok))
visit (UnaryOpNode tok ast) = do
    let ast_rt = visit ast
    if is_success ast_rt then do
        let num = get_num ast_rt
        case tok of
            TPlus -> (RTSuccess num)
            TMinus -> (RTSuccess (mult (Float (-1)) num))
            _ -> (RTFailure (RuntimeError "Invalid operator token for UnaryOpNode"))
    else
        ast_rt

visit (BinOpNode tok left right) = do
    let left_rt = visit left
    let right_rt = visit right
    if (is_success left_rt) && (is_success right_rt) then do
        let left = (get_num left_rt)
        let right = (get_num right_rt)
        case tok of
            TPlus -> (RTSuccess (add left right))
            TMinus -> (RTSuccess (sub left right))
            TMult -> (RTSuccess (mult left right))
            TDiv -> (RTSuccess (mDiv left right))
            _ -> (RTFailure (RuntimeError "Unrecognised token"))
    else
        left_rt

visit _ = (RTFailure (RuntimeError "Node implementation not implemented"))

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