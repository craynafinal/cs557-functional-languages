product1 :: [Int] -> Int
product1 [] = 1
product1 l = (head l) * product1(tail l)

qsort [] = []
qsort (x:xs) = 	qsort smaller ++ [x] ++ qsort larger
								where
									smaller = [a | a <- xs, a <= x]
									larger = [b | b <- xs, b > x]

qsortrev l = reverse(qsort l)
