import Data.List
import Data.Maybe
import qualified Graphics.GD as GD

-- Color in R G B format
-- values ranges from 0 to 1
data Color = Color Float Float Float deriving (Show)

data Transformation = Transformation {
    a11 :: Float, a12 :: Float, a13 :: Float, a14 :: Float,
    a21 :: Float, a22 :: Float, a23 :: Float, a24 :: Float,
    a31 :: Float, a32 :: Float, a33 :: Float, a34 :: Float,
    a41 :: Float, a42 :: Float, a43 :: Float, a44 :: Float
} deriving (Show)

stay :: Transformation
stay = Transformation
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1

translate :: Float -> Float -> Float -> Transformation
translate x y z = Transformation
    1 0 0 (-x)
    0 1 0 (-y)
    0 0 1 (-z)
    0 0 0 1

-- Vector with homogeneous coordinates
-- x, y, z, h
data Vector = Vector Float Float Float Float deriving (Show)

origo = Vector 0 0 0 1

(<*>) t (Vector x y z h) = Vector
    ((a11 t)*x+(a12 t)*y+(a13 t)*z+(a14 t)*h)
    ((a21 t)*x+(a22 t)*y+(a23 t)*z+(a24 t)*h)
    ((a31 t)*x+(a32 t)*y+(a33 t)*z+(a34 t)*h)
    ((a41 t)*x+(a42 t)*y+(a43 t)*z+(a44 t)*h)

infixl 7 <*>

(<+>) :: Vector -> Vector -> Vector
(<+>) (Vector x1 y1 z1 h1) (Vector x2 y2 z2 h2) = Vector (x1+x2) (y1+y2) (z1+z2) 1

infixl 6 <+>

(<->) :: Vector -> Vector -> Vector
(<->) (Vector x1 y1 z1 h1) (Vector x2 y2 z2 h2) = Vector (x1-x2) (y1-y2) (z1-z2) 1

infixl 6 <->

normalize :: Vector -> Vector
normalize (Vector x y z h) = Vector (x/h) (y/h) (z/h) 1

data Shape =
    Cube  -- Unit cube 
    | Sphere -- Unit sphere
    deriving (Show)


data Finish = Finish {
    finish_ambient_color :: Color,
    finish_diffuse_color :: Color,
    finish_reflect_color :: Color,
    finish_filter        :: Float, -- 0: opaque, 1: transparent
    finish_shininess     :: Float, -- 0: diffuse, 1: shiny
    finish_refraction    :: Float  -- 1: air
} deriving (Show)


data Object = Object {
    object_shape  :: Shape,
    object_finish :: Finish,
    object_trans :: Transformation
} deriving (Show)


data Light = Light {
    light_position :: Vector,
    light_color :: Color
} deriving (Show)


-- When a ray hit something, it expressed
-- with an Incidence object
data Incidence = Incidence {
    incidence_object :: Object,
    incidence_vector  :: Vector,
    incidence_normal :: Vector
} deriving (Show)


-- Represents a ray (semi line)
data Ray = Ray {
    ray_origin :: Vector,
    ray_direction :: Vector
} deriving (Show)

-- Camera is always at (0,0,0)
-- and the canvas center is always at (0,0,1)
data Camera = Camera {
    camera_canvas_width :: Float,
    camera_canvas_height :: Float,
    camera_resolution :: Float -- 1/pixel
} deriving (Show)


-- Represents a scene (list of objects)
data Scene = Scene {
    scene_objects :: [Object], -- List of objects
    scene_ambient :: Color,    -- Ambient light
    scene_background :: Color -- Background color
} deriving (Show)


-- Solves a quadratic equation. The result is the list of
-- distinct solutions (two element, one element, or empty list)
solveQuadratic :: Float -> Float -> Float -> [Float]
solveQuadratic a b c
    | d <  0    = []
    | d == 0    = [(solveWithFunc (+))]
    | otherwise = map solveWithFunc [(+),(-)]
    where
        d = b**2 - 4*a*c
        solveWithFunc f = (-b `f` sqrt(d))/(2*a)


-- Calculates the length of a vector
-- does not care about the h coordinate, it must be 1!
vlen :: Vector -> Float
vlen (Vector x y z h) = sqrt(x**2+y**2+z**2)

-- Returns the closest point to a given point from a point list
closestVector :: Vector -> [Vector] -> Vector
closestVector v vectors = foldl1' (\v1 v2 -> if vlen (v2<->v) < vlen (v1<->v) then v2 else v1) vectors


-- Returns the closest object to a ray base in the direction of the ray
-- Todo: we should work with shapes here, not objects
-- This takes care of the object transformations
firstHit :: Ray -> [Object] -> Maybe Incidence
firstHit ray objects = foldl' (closerIncidence ray) Nothing objects
    where
        closerIncidence :: Ray -> Maybe Incidence -> Object -> Maybe Incidence
        closerIncidence r@(Ray v _) i1 o2
            | isNothing i1 = i2
            | isNothing i2 = i1
            | d1 < d2      = i1
            | otherwise    = i2
            where
                i2 = incidence (Ray (object_trans o2 <*> ray_origin r) (ray_direction r)) o2 -- Todo: refactor this to separate function
                d1 = vlen $ v <-> (incidence_vector $ fromJust i1)
                d2 = vlen $ v <-> (incidence_vector $ fromJust i2)


-- Calculates the intersection of a ray and a shape.
-- Returns the closest intersection to the starting
-- Vector of the ray.
incidence :: Ray -> Object -> Maybe Incidence

-- Incidence with a unit sphere at origo
incidence
    (Ray (Vector rx ry rz _) (Vector rdx rdy rdz _))
    obj@(Object Sphere _ t) =

    if isNothing vector
    then Nothing
    else Just $ Incidence obj (fromJust vector) (Vector px py pz 1)
    where
        (Vector px py pz _) = fromJust vector
        vector =
            if solutions == []
            then Nothing
            else
                let x = minimum solutions
                in Just $ Vector (x*rdx+rx) (x*rdy+ry) (x*rdz+rz) 1
        solutions = filter (> 0) $ solveQuadratic
            (rdx**2+rdy**2+rdz**2)
            (2*rx*rdx+2*ry*rdy+2*rz*rdz)
            (rx**2+ry**2+rz**2-1)

-- Intersection with a cube
incidence
    (Ray (Vector rx ry rz _) (Vector rdx rdy rdz _))
    (Object Cube _ _) =

    Nothing


-- Tell the color seen by a single ray
renderPixel :: Ray -> Scene -> Color

renderPixel ray (Scene objs amb bg)
    | isNothing i = bg
    | otherwise   = let Just (Incidence (Object _ (Finish c _ _ _ _ _) _) _ _) = i in c
    where
        i = firstHit ray objs

-- Returns coordinates on the image, and the rays through those
-- coordinates
rays :: Camera -> [((Int,Int),Ray)]
rays (Camera w h r) =
    [
        ((x, y), Ray origo (Vector (-w/2+fromIntegral(x)/r) (-h/2+fromIntegral(y)/r) 1 1) )
        | x <- [0..round(w*r)], y <- [0..round(h*r)]
    ]

render :: Scene -> Camera -> [((Int, Int),Color)]

render scene cam = [((x, y), renderPixel ray scene) | ((x, y), ray) <- rays cam]


-- Execute main program.
main :: IO ()

-- Get the list of coordinates and associated colors and call GD's
-- setPixel function on each of them using a freshly created image.
main = do
    image <- GD.newImage (500, 500)
    mapM_
        (\((x, y), (Color r g b)) -> GD.setPixel (x, y) (GD.rgb (round (r*256)) (round (g*256)) (round (b*256))) image)
        (render
            (Scene
                [Object Sphere (Finish (Color 0 0.7 0) (Color 0 0 0) (Color 0 0 0) 0 0 0) (translate 0 0 2)]
                (Color 0.7 0.7 0.7)
                (Color 0.3 0.3 0.8)
            )
            (Camera 20 20 25)
        )
    GD.savePngFile "raytracer.png" image
