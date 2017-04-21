group :: Int -> [a]  -> [[a]]
group n =  takeWhile (not . null)
	. map (take n)
	. iterate (drop n)

map1 = map . map

map2 = (map, map)

map3 = [map id, reverse, map not]
