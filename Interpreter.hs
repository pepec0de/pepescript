visit :: AST -> RuntimeResult
visit (NumNode tok) = (RTSuccess (get_token_number tok))
visit (BinOpNode tok left right)
    | tok == TPlus = do
        let left_rt = visit left
        let right_rt = visit right
        if (is_success left_rt) && (is_success right_rt) then
            (RTSuccess ((get_num left_rt) + (get_num right_rt)))
        else
            (RTFailure "error adding")
    | otherwise = (RTFailure "eroerka")
visit (UnaryOpNode tok ast) = False
visit _ = False

get_token_number :: Token -> Num
get_token_number (TInt num) = num
get_token_number (TFloat num) = num

is_success :: RuntimeResult -> Bool
is_success (RTSuccess _) = True
is_success _ = False

get_num :: RuntimeResult -> Num
get_num (RTSuccess num) = num