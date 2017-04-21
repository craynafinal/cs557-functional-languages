test2 [] ys = []
test2 (x:xs) ys = (map (+x) ys) : (test2 xs ys)

test n = concat ((map (+1) [-1,-2..(-n)]) : (test2 [2..(n)] [-1,-2..(-n)]))
