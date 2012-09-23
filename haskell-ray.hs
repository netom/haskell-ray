import qualified Graphics.GD as GD

-- Represents a color in R G B format
-- values ranges from 0 to 1
data Color = Color Float Float Float deriving (Eq, Ord, Show)

-- Represents a point in the 3D space
-- x y z
data Point = Point Float Float Float deriving (Eq, Ord, Show)

-- Represents a direction (e.g a vector)
-- x y z
data Direction = Direction Float Float Float deriving (Eq, Ord, Show)

-- Represents one of 
data Shape =
    Sphere Point Float                     -- Place, radius
    | Cube Point Direction Direction Float   -- Place, face1 normal, face2 normal, edge size
    | Square Point Direction Direction Float -- Place, face normal, edge normal, edge size
    deriving (Eq, Ord) -- Ugly

-- Represents a material
-- opaqueness, reflectivity, trans. color, 
data Material = Material
    Float -- Opaqueness, 0: opaque, 1: transculent
    Float -- Reflectivity, 0: diffuse, 1: perfect mirror
    Color -- Diffuse tint
    Color -- Reflection tint
    Color -- Inner tint
    deriving (Eq, Ord) -- Ugly.

-- Represents an object that has a shape and a material
data Object = Object Shape Material deriving (Eq, Ord) -- Ugly. refactor firstHit

-- Represents a ray (semi line)
data Ray = Ray Point Direction

-- Represents a scene (list of objects)
data Scene = Scene
    [Object] -- List of objects
    Color    -- Ambient light
    Color    -- Background color

-- Turns a color into an integer palatable by GF
color2GDColor :: Color -> GD.Color
color2GDColor (Color r g b) = 255*255*round(255*r)+255*round(255*g)+round(255*b)

-- Solves a quadratic equation. The result is the list of
-- distinct solutions (two element, one element, or empty list)
solveQuadratic :: Float -> Float -> Float -> [Float]
solveQuadratic a b c
    | d <  0    = []
    | d == 0    = [(solveWithFunc a b c (+))]
    | otherwise = map (solveWithFunc a b c) [(+),(-)]
    where
        d = b**2 - 4*a*c
        solveWithFunc a b c f = (-b `f` sqrt(d))/(2*a)

-- Calculates the distance of two points in 3D space
distance :: Point -> Point -> Float
distance (Point x1 y1 z1) (Point x2 y2 z2) =
    sqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)

-- Returns the closest point to a given point from a point list
closestPoint :: Point -> [Point] -> Point
closestPoint p points = snd . minimum $ (map (\x -> (distance p x, x)) points)

-- Returns the closest object to a ray base in the direction of the ray
-- Todo: we should work with shapes here, not objects
firstHit :: Ray -> [Object] -> Maybe Object
firstHit r@(Ray p _) ol
    | distances == [] = Nothing
    | otherwise       = Just $ snd . minimum $ zip (distances) ol
    where
        distances = map (\(Just p)->p) (filter (/= Nothing) (map (intersection r . (\(Object s _) -> s)) ol))

-- Calculates the intersection of a ray and a shape.
-- Returns the closest intersection to the starting
-- point of the ray.
--
-- Maybe we need an intersection type that can
-- distinguish between wether a shape was hit
-- from the inside or from the outside?
intersection :: Ray -> Shape -> Maybe Point

-- Intersection with a sphere
-- We move the ray and sphere so that the sphere's center is
-- at (0,0,0). This way the quadratic equation is simpler.
intersection (Ray rp rd) (Sphere sp r) =
    if points == []
    then Nothing
    else Just $ closestPoint rp points -- The quadratic equation "knows" this. Refactor!
    where
        Point rx ry rz = rp        -- Ray position
        Direction rdx rdy rdz = rd -- Ray direction
        Point sx sy sz = sp        -- Sphere position
        (tx, ty, tz) = (rx-sx, ry-sy, rz-sz) -- Translated ray position
        points = map (\x -> Point (x*rdx+rx) (x*rdy+ry) (x*rdz+rz)) solutions
        solutions = filter (> 0) $ solveQuadratic
            (rdx**2+rdy**2+rdz**2)
            (2*tx*rdx+2*ty*rdy+2*tz*rdz)
            (tx**2+ty**2+tz**2-r**2)

-- Intersection with a square sheet
intersection (Ray rp rd) (Square sp sfn sen e) = Nothing

-- Intersection with a cube
intersection (Ray rp rd) (Cube cp cd1 cd2 e) = Nothing

renderPixel :: Ray -> Scene -> Color
renderPixel ray (Scene objs amb bg)
    | int == Nothing = bg
    | otherwise   = let Just (Object _ (Material _ _ c _ _)) = int in c
    where
        int = firstHit ray objs

render cp cd scene image (Point ix iy iz) s1 s2 = 
    mapM_
        (\(x, y) -> GD.setPixel (x, y) (color2GDColor $ renderPixel (Ray cp (Direction (ix+(fromIntegral x)*s1) (iy+(fromIntegral y)*s2) iz)) scene) image)
        [(x, y) | x <- [0..499], y <- [0..499]]

main = do
    image <- GD.newImage (500, 500)
    -- Camera pos, direction, ambient light, object, object color, background colorimage
    render
        (Point 0 0 0)     -- Camera position
        (Direction 0 0 1) -- Camera direction
        (Scene
            [Object
                (Sphere (Point 0 0 10) 2)
                (Material 0 0 (Color 0 0.7 0) (Color 0 0 0) (Color 0 0 0))] -- Green sphere
            (Color 0.7 0.7 0.7) -- Ambient white light
            (Color 0.3 0.3 0.8) -- Background is light green
        )
        image             -- Image object
        (Point (-10) 10 5)  -- Image upper left corner position
        0.04  -- Image "right" scale
        (-0.04) -- Image "down" scale
    GD.savePngFile "haskell-ray.png" image
