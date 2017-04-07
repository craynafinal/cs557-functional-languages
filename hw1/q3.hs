powerOfTwo :: Int -> Integer
powerOfTwo n = product (take n (repeat 2))

logTwo :: Integer -> Int
logTwo v = fromIntegral (last [x | x <- [1..v], (powerOfTwo (fromInteger x)) <= v])

copy :: Int -> a -> [a]
copy n x = take n (repeat x)
