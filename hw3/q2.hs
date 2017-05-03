import Data.List

layout :: [String] -> IO ()
layout  = putStr
	. unlines
	. zipWith (\n l -> show n ++ ") " ++ l) [1..]

appString    :: String -> String -> String
appString l r = "(" ++ l ++ "++" ++ r ++ ")"

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

data Tree = Leaf String | Node Tree Tree
	deriving Show

stringToTree :: String -> [Tree]
stringToTree x = case split x of
	[] -> [Leaf x]
	otherwise -> [Node l r | y <- split x, l <- (stringToTree (fst y)), r <- (stringToTree (snd y))]

treeToAppString :: Tree -> String
treeToAppString (Leaf x) = x
treeToAppString (Node x y) = appString (treeToAppString x) (treeToAppString y)

allWays :: String -> [String]
allWays x = x : (map treeToAppString (stringToTree x))

noParens   :: String -> [String]
noParens xs = nub (map (filter (not . (`elem` "()"))) ((allWays xs)++(otherOnes (split xs))))

otherOnes :: [(String,String)] -> [String]
otherOnes [] = []
otherOnes (x:xs) = (appString (fst x) (snd x)) : (otherOnes xs)
