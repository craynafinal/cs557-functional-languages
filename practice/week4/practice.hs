data Type = T | S | O Type Type
	deriving Show

splits :: [a] -> [([a],[a])]
splits ts = zip (inits ts) (tails ts)

inits :: [a] -> [[a]]
inits [x] = []
inits (x:xs) = map (x:) ([]:inits xs)

tails :: [a] -> [[a]]
tails [x] = []
tails (x:xs) = xs : tails xs

alltypes :: [Type] -> [Type]
alltypes [t] = [t]
alltypes ts = [O l r | (ls,rs) <- splits ts, l <- alltypes ls, r <- alltypes rs]
