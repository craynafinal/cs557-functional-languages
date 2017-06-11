module VizTree where

import Treedot

data VizTree = VizNode String [VizTree]

instance Tree (VizTree) where
	subtrees (VizNode x xs) = xs

instance LabeledTree (VizTree) where
	label (VizNode x xs) = x

class Viz a where
	toVizTree :: a -> VizTree
	-- handling a list is implemented under Viz class instead of the instance with a list param
	toVizList :: [a] -> VizTree
	toVizList [] = VizNode "[]" []
	toVizList (x:xs) = VizNode ":" [toVizTree x, toVizTree xs]

instance Viz Integer where
	toVizTree n = VizNode (show n) []

instance Viz Char where
	toVizTree n = VizNode (show n) []
	-- when it sees Char of param, this is going to use this overriden definition to handle special case
	toVizList s = VizNode ("\\\""++s++"\\\"") []

instance Viz a => Viz [a] where
	-- otherwise it is going to use the class method
	toVizTree = toVizList
--	toVizTree [] = VizNode "[]" []
--	toVizTree (x:xs) = VizNode ":" [toVizTree x, toVizTree xs]

instance Viz Bool where
	toVizTree n = VizNode (show n) []

instance Viz Int where
	toVizTree n = VizNode (show n) []

instance Viz a => Viz (Maybe a) where
	toVizTree Nothing = VizNode "Nothing" []
	toVizTree (Just a) = toVizTree a

instance (Viz a, Viz b) => Viz (a,b) where
	toVizTree (a,b) = VizNode ((label (toVizTree a)) ++ "," ++ (label (toVizTree b))) []

instance (Viz a, Viz b, Viz c) => Viz (a,b,c) where
	toVizTree (a,b,c) = VizNode ((label (toVizTree a)) ++ "," ++ (label (toVizTree b)) ++ "," ++ (label (toVizTree c))) [] 

viz :: Viz a => a -> IO ()
viz = writeFile "tree.dot" . toDot . toVizTree

vizRest :: Viz a => a -> String
vizRest = toDot . toVizTree
