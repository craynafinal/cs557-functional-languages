bools :: [Bool]
bools = [True]

nums :: [[Int]]
nums = [[1]]

add :: Int -> Int -> Int -> Int
add x y z = x+y+z

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply f a = f a

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,a) -> (a,a)
swap (x,y) = (y,x)

pair :: a -> a -> (a,a)
pair x y = (x,y)

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
