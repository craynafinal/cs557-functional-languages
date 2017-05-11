box :: a -> [a]
box x = [x]

f :: [a] -> Bool
f xs = null xs || f [xs]
