powerOfTwo :: Int -> Integer
powerOfTwo n = product (replicate n 2)

logTwo :: Integer -> Int
logTwo v = fromIntegral (last [x | x <- [1..v], (powerOfTwo (fromInteger x)) <= v])
