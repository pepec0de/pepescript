# pepescript
A new interpreter built on Haskell

### TODO List
* Fix troubleshooting with command: "let"

### Pepescript Grammar

```
expr        : TKeyword:let IDENTIFIER TEq expr
            : comp-expr ((AND | OR) comp-expr)*

comp-expr   : NOT comp-expr
            : arith-expr ( ( == | <(=) | >(=) ) arith-expr )*

arith-expr  : term ((PLUS | MINUS) term)* -> BinOpNode [+, -]

term        : factor ((MUL | DIV) factor)* -> BinOpNode [*, /]

factor      : (PLUS | MINUS) factor
            : power

power       : atom (POW factor)*

atom        : INT | FLOAT -> NumNode
            : LPAREN expr RPAREN
            : if-expr
            : while-expr

if-expr     : if expr then expr
                (else expr)?

while-expr  : while expr then expr
```