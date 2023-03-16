# tulip
A small functional programming language

## Language constructs
**Global declarations** can either be function declarations or data declarations.
Function declarations can contain arguments separated spaces on the left side, the right side of the '=' is a expression.

Data declarations also use the '=' sign, but can't have arguments. They use the 'data' keyword followed by a number indicating how many arguments this constructor shall take.

```
main = 5;
plus5 x = x + 5;
nil = data 0;
cons = data 2;
```

Expressions can be one of:
- a case expression
- an application of two expressions
- an identifier
- a constant

Examples for each (assigned to some name):
```
# case expression
# cases are separated by a comma, some constructors take arguments, e.g. cons, these arguments can be namend here, just like in haskell
nilIsNil = case nil of
  nil -> 0,
  cons x xs -> 1;
  
# application
# every application is in prefix notation, so even built-ins like add need to be written in prefix notation.
# there is no list of built-in functions yet.
ten = add 5 5;

# identifier
x = nil;

# constant
n = 5;
```
