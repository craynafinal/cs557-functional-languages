type Point = (Float,Float)
type Grid a = [[a]]
type Image color = Point -> color

next :: Point -> Point -> Point
next (u,v) (x,y) = (x*x-y*y+u, 2*x*y+v)

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (0,0)
-- applying (next p) to (0,0) iteratively

fairlyClose :: Point -> Bool
fairlyClose (u,v) = (u*u + v*v) < 100

inMandelbrotSet :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mandelbrot p)
-- grabbing only ones filtered by fairlyClose

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

mandGrid = grid 79 37 (-2.25, -1.5)  (0.75, 1.5)
juliaGrid = grid 79 37 (-1.5, -1.5) (1.5, 1.5)

sample :: Grid Point -> Image color -> Grid color
sample points image = map (map image) points

draw :: [color] -> Grid Point -> (Grid color -> pic) -> pic
draw palette grid render
	= render (sample grid (fracImage palette))

charPalette :: [Char]
charPalette = "    ,.`\"~:;o-!|?/<>X+={^O#%&@8*$"

charRender :: Grid Char -> IO ()
charRender = putStr . unlines

example1  = draw charPalette mandGrid charRender
