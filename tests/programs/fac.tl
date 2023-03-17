if cond then else = case cond of
    true -> then,
    false -> else;

fac x = if (eq x 1)
            1
            (mul x (fac (sub x 1)));
main = fac 5;
