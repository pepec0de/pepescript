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

if-expr     : if expr LBRACKET expr RBRACKET -> IfNode
                (else expr)?

while-expr  : while expr LBRACKET statements RBRACKET -> WhileNode
``` 

### Examples

* Fibonacci(n):
```
let n = 6; 
let a = 0; 
let fib = 1; 
let c = 0;

while n != 1 { 
	let c = a + fib;
	let a = fib; 
	let fib = c; 
	let n = n - 1
}
```

* Factorial(n):
```
let n = 5;
let r = 1;
let i = 1;

while i <= n {
    let r = r * i;
    let i = i + 1
}
```