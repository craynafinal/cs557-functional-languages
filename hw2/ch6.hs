merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x :(merge xs (y:ys))
	| otherwise = y : (merge (x:xs) ys)

midPoint :: [a] -> Int
midPoint xs = (length xs) `div` 2

halve :: [a] -> ([a],[a])
halve xs = (take (midPoint xs) xs, drop (midPoint xs) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst (halve xs))) (msort (snd (halve xs))) 
