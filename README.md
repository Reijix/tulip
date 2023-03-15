# tulip
A small functional programming language

## Language constructs
**Global declarations** are just assigning a name to an expression. The left side of a '=' can only contain a name, not arguments.
Every declaration ends with a semicolon.

```
main = 5;
```

Expressions can be one of:
- a constructor
- a case expression
- an application of two expressions
- a lambda abstraction
- an identifier
- a constant

Examples for each (assigned to some name):
```
# constructors
nil = data 0; # a constructor that takes no arguments
cons = data 2; # a constructor that takes two arguments

# case expression
# cases are separated by a comma, some constructors take arguments, e.g. cons, these arguments can be namend here, just like in haskell
nilIsNil = case nil of
  nil -> 0,
  cons x xs -> 1;
  
# application
# every application is in prefix notation, so even built-ins like add need to be written in prefix notation.
# there is no list of built-in functions yet.
ten = add 5 5;

# lambda abstraction
# lambdas are written in the usual style, as of right now a lambda can only take one argument (so you need to chain lambdas).
plusOne = \x -> add x 1

# identifier
x = nil;

# constant
n = 5;
```
