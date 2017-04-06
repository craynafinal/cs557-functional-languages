type Point = (Float,Float)

next :: Point -> Point -> Point
next (u,v) (x,y) = (x*x-y*y+u, 2*x*y+v)

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (0,0)

fairlyClose :: Point -> Bool
fairlyClose (u,v) = (u*u + v*v) < 100

inMandelbrotSet :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mandelbrot p)
