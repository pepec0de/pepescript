module Types where

type Position = Int -- En un futuro: (Int, Int) (<Columna>, <Fila>)

data Token = 
    TKeyword_let        |
    TKeyword_while      |
    TKeyword_if         |
    TKeyword_then       |
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

-- Methods for tokens
is_identifier :: Token -> Bool
is_identifier (TIdentifier _) = True
is_identifier _ = False

type Case = (AST, AST) -- (<Condition>, <Expression>)
data AST =
    Empty |
    NumNode Token           | -- NumNode (Number)
    BinOpNode Token AST AST | -- BinOpNode(Operator, leftTree, rightTree)
    UnaryOpNode Token AST   | -- UnaryOpNode(Operator, Tree)
    VarAccessNode Token     | -- VarAccessNode(Operator, var Token)
    VarAssignNode Token AST | -- VarAssignNode(var Identifier, Expression)
    IfNode Case AST       | -- IfNode(Case, ElseCase)
    WhileNode AST AST
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