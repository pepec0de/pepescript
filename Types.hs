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
    TNot | -- !
    TEq | -- =
    TEqEq | -- ==
    TNotEq | -- !=
    TGt | -- >
    TGtEq | -- >=
    TLt | -- <
    TLtEq | -- <=
    TEOF
    deriving (Eq, Show)

data AST =
    Empty |
    NumNode Token | -- NumNode (Number)
    BinOpNode Token AST AST | -- BinOpNode(Operator, left tree, right tree)
    UnaryOpNode Token AST -- UnaryOpNode(Operador, tree)
    deriving (Eq, Show)

data Error =
    None |
    IllegalCharError String |
    InvalidSyntaxError String |
    RuntimeError String
    deriving (Eq, Show)

data ParseResult =
    Success AST |
    Failure Error
    deriving (Eq, Show)

data Number =
    Int Int |
    Float Float
    deriving (Eq, Show)

data RuntimeResult =
    RTSuccess Number |
    RTFailure Error 
    deriving (Eq, Show)

data Context = NoParent | Context String Context -- display_name, parent_context
    deriving (Eq, Show)