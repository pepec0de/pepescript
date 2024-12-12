module Types where

type Position = Int -- En un futuro: (Int, Int) (<Columna>, <Fila>)

data Token = 
    TInt Int |
    TFloat Float |
    TPlus |
    TMinus |
    TMult |
    TDiv |
    TLParen |
    TRParen |
    TEOF
    deriving (Eq, Show)

data AST =
    Empty |
    NumNode Token |
    BinOpNode AST Token AST
    deriving (Show)

data Error =
    None |
    IllegalCharError String |
    IllegalSyntaxError String
    deriving (Eq, Show)