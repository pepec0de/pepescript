# pepescript
A new interpreter built on Haskell

### TODO List
* ~Fix troubleshooting with command: "let"~

### Pepescript Grammar

```
statements  : SEMICOLON* expr (SEMICOLON expr)* -> multiple lines

expr        : TKeyword:let IDENTIFIER TEq expr -> VarAssignNode
            : comp-expr ((AND | OR) comp-expr)* -> BinOpNode [&&, ||]

comp-expr   : NOT comp-expr
            : arith-expr ( ( == | <(=) | >(=) ) arith-expr )*

arith-expr  : term ((PLUS | MINUS) term)* -> BinOpNode [+, -]

term        : factor ((MUL | DIV) factor)* -> BinOpNode [*, /]

factor      : (PLUS | MINUS) factor -> UnaryOpNode
            : power

power       : call (POW factor)* -> BinOpNode [^]

call        : atom (LPAREN (expr (COMMA expr)*)? RPAREN -> CallNode )?

atom        : INT | FLOAT -> NumNode
            : LPAREN expr RPAREN
            : IDENTIFIER -> VarAccessNode
            : if-expr
            : while-expr

if-expr     : if expr then statements -> IfNode
                (else expr)?

while-expr  : while expr then statements -> WhileNode
```