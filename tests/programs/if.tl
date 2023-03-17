true = data 0;
false = data 0;
if cond then else = case cond of
    true -> then,
    false -> else;

main = if (eq 5 4) 10 20;