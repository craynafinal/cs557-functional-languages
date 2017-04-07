dup :: (a -> a -> a) -> a -> a
dup f x = f x x 

double :: Integer -> Integer
double n = dup (+) 2

square :: Integer -> Integer
square n = dup (*) n 
