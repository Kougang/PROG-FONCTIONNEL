data Shape = Circle Radius 
            |Rect Side Side
            deriving Show
type Radius = Float
type Side = Float

--Definition of square function
square:: Float -> Shape
square n = Rect n n

area:: Shape -> Float
area (Circle r) = 2*pi*r
area(Rect a b) = a * b