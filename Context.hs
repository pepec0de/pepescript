module Context where

import Types

type Symbol = (String, Number)
data Context = NoParent | Context String Context [Symbol] -- display_name, parent_context
    deriving (Eq, Show)

find_var :: String -> Context -> (Bool, Number)
find_var identifier context = find_symbol identifier (get_symbol_table context)

find_symbol :: String -> [Symbol] -> (Bool, Number)
find_symbol _ [] = (False, Float 0)
find_symbol identifier ((str, num):symbols) = 
    if identifier == str then
        (True, num)
    else 
        find_symbol identifier symbols

get_symbol_table :: Context -> [Symbol]
get_symbol_table (Context _ _ symbol_table) = symbol_table

add_var_to_context :: Context -> String -> Number -> Context
add_var_to_context (Context name parent symbol_table) var num = 
    Context name parent (add_var symbol_table var num)

add_var :: [Symbol] -> String -> Number -> [Symbol]
add_var symbol_table var num = (var, num):symbol_table
