isPerfect :: Int -> Int
isPerfect x = sum [y | y <- [1..x], x /= y, x `mod` y == 0]

perfects :: Int -> [Int]
perfects x = [y | y <- [1..x], (isPerfect y) == y]

scalaproduct :: [Int] -> [Int] -> Int
scalaproduct xs ys = sum [a * b | (a, b) <- zip xs ys]
