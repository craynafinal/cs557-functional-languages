import Data.List

integers :: [Integer]
integers  = 0 : [x | y <- [1..10], x <- [y, -y]]

pairs :: [(Integer, Integer)]
pairs = [(x, y-x) | y <- [0..], x <- [0..y]]

pos :: (Integer, Integer) -> Integer
pos (x, y) = sum (map (+1) [0..(x + y)]) - y - 1
