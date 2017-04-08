type Point = (Float,Float)
type Grid a = [[a]]
type Image color = Point -> color

{--
Modified the next function by adding constant multipliers
for the left value of the pointer and increased multiplier
from 2 to 3 on the right value.
--}
next :: Point -> Point -> Point
next (u,v) (x,y) = (2*x*x-1.1*y*y+u, 3*x*y+v)

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (0,0)

fairlyClose :: Point -> Bool
fairlyClose (u,v) = (u*u + v*v) < 100

fracImage :: [color] -> Point -> color
fracImage palette = (palette!!)
	.length
	.take n
	.takeWhile  fairlyClose
	.mandelbrot
	where n = length palette - 1


grid :: Int -> Int -> Point -> Point -> Grid Point
grid c r (xmin, ymin) (xmax, ymax)
	= [[ (x,y) 	| x <- for c xmin xmax ]
							| y <- for r ymin ymax ]

for :: Int -> Float -> Float -> [Float]
for n min max = take n [ min, min+delta .. ]
	where delta = (max-min) / fromIntegral (n-1)

{--
Modified the grid size and the range of points.
Slightly increased the grid size from 79, 37 to 90, 50.
Changed xmax from 1.5 to 1.
--}
jongGrid = grid 90 50 (-1.5, -1.5) (1, 1.5)

sample :: Grid Point -> Image color -> Grid color
sample points image = map (map image) points

draw :: [color] -> Grid Point -> (Grid color -> pic) -> pic
draw palette grid render
	= render (sample grid (fracImage palette))

{--
Modified a set of characters to be printed.
--}
charPalette :: [Char]
charPalette = "    ^*()[]+-/=<>!@#$%&1234567890"

charRender :: Grid Char -> IO ()
charRender = putStr . unlines

example = draw charPalette jongGrid charRender

