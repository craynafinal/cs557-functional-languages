strange xs = head (head (reverse (takeWhile notnull (iterate twirl xs))))
notnull xs = not (null xs)
twirl xs = reverse (tail xs)

strangefix :: [a] -> a
strangefix xs = xs !! (quot (length xs) 2)
