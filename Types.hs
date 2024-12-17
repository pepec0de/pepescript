module Types where

type Position = Int -- En un futuro: (Int, Int) (<Columna>, <Fila>)

data Token = 
    TKeyword_let        |
    TKeyword_while      |
    TKeyword_if         |
    TIdentifier String  |
    TInt Int            |
    TFloat Float        |
    TPlus               |
    TMinus              |
    TMult               |
    TDiv                |
    TPow                | -- ^
    TLParen             | -- (
    TRParen             | -- )
    TNot                | -- !
    TEq                 | -- =
    TEqEq               | -- ==
    TNotEq              | -- !=
    TGt                 | -- >
    TGtEq               | -- >=
    TLt                 | -- <
    TLtEq               | -- <=
    TAnd                | -- &&
    TOr                 | -- ||
    TEOF
    deriving (Eq, Show)

data AST =
    Empty |
    NumNode Token | -- NumNode (Number)
    BinOpNode Token AST AST | -- BinOpNode(Operator, left tree, right tree)
    UnaryOpNode Token AST | -- UnaryOpNode(Operador, tree)
    VarAccessNode Token |
    VarAssignNode Token AST -- VarAssignNode(Identifier, Expression)
    deriving (Eq, Show)

data Error =
    None |
    IllegalCharError String |
    InvalidSyntaxError String |
    RuntimeError String
    deriving (Eq, Show)

data ParseResult =
    ParseSuccess AST |
    ParseFailure Error
    deriving (Eq, Show)

data Number =
    Int Int |
    Float Float
    deriving (Eq, Show)

data RuntimeResult =
    RTSuccess Number |
    RTFailure Error 
    deriving (Eq, Show)