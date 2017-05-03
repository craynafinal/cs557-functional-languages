isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:xs) = (x <= head(xs)) && isSorted(xs)

isRevSorted :: Ord a => [a] -> Bool
isRevSorted xs = isSorted(reverse(xs))

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

revMsort :: Ord a => [a] -> [a]
revMSort [] = []
revMsort xs = reverse (msort xs)

testSorter :: ([Int] -> [Int]) -> Int -> Bool
testSorter f n = isSorted(f [1..n])
