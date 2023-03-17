# list constructors
nil = data 0;
cons = data 2;

# do something with them
length l = case l of
    nil -> 0,
    cons x xs -> add 1 (length xs);

main = length (cons 5 (cons 10 nil));
