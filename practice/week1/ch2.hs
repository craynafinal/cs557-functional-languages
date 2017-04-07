last1 :: [a] -> a
last1 l = head(reverse l)

init1 :: [a] -> [a]
init1 l = reverse (tail (reverse l))

init2 :: [a] -> [a]
init2 l = take ((length l) - 1) l
