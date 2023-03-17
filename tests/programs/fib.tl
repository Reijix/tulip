if cond then else = case cond of
    true -> then,
    false -> else;

fib n = if (le n 2)
            1
            (add (fib (sub n 1)) (fib (sub n 2)));

main = fib 15;