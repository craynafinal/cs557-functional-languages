import IO

gauss :: IO Int
gauss = return (sum[1..10])

data Parser a = P (String -> [(a,String)])

empty :: Parser a
empty = P (\s -> [])

item :: Parser Char
item = P (\s -> case s of
									[] -> []
									(c:cs) -> [(c,cs)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) s = p s

instance Functor Parser where
	fmap f (P p) = P (\s -> [(f x, y) | (x, y) <- p s])

--return :: a -> Parser a
--return x = P(\s -> [(x,s)])

--return :: a-> Maybe a
--return x = Just x

