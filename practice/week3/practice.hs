type Pos = (Int, Int)

data Move = North | South | East | West

data Maybe a = Nothing | Just a
	deriving Show

data Nat = Zero | Succ Nat
	deriving Show

data Op = Add | Sub | Mul | Div
instance Show Op where
	show Add = "+"
	show Sub = "-"
	show Mul = "*"
	show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
	show (Val n) = show n
	show (App o l r) = brak l ++ show o ++ brak r
		where
			brak (Val n) = show n
			brak e = "(" ++ show e ++ ")"

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
	where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

