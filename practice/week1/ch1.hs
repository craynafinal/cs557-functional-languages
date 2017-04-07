product1 :: [Int] -> Int
product1 [] = 1
product1 l = (head l) * product1(tail l)
