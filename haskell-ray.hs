import Data.List
import Data.Maybe

-- Color in R G B format
-- values ranges from 0 to 1
data Color = Color {
    color_r :: Float,
    color_g :: Float,
    color_b :: Float
} deriving (Show)


data Transformation = Transformation {
    t11 :: Float,
    t12 :: Float,
    t13 :: Float,
    t21 :: Float,
    t22 :: Float,
    t23 :: Float,
    t31 :: Float,
    t32 :: Float,
    t33 :: Float
}

class Transformable a where
    transform :: Transformation -> a -> a

data Point = Point {
    point_x :: Float,
    point_y :: Float,
    point_z :: Float
} deriving (Show)

instance Transformable Point where
    transform t p@(Point x y z) = p

data Vector = Vector {
    vector_x :: Float,
    vector_y :: Float,
    vector_z :: Float
} deriving (Show)


instance Transformable Vector where
    transform t v@(Vector x y z) = v

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
    object_finish :: Finish
} deriving (Show)


data Light = Light {
    light_position :: Point,
    light_color :: Color
} deriving (Show)


-- When a ray hit something, it expressed
-- with an Incidence object
data Incidence = Incidence {
    incidence_object :: Object,
    incidence_point  :: Point,
    incidence_normal :: Vector
} deriving (Show)


-- Represents a ray (semi line)
data Ray = Ray {
    ray_origin :: Point,
    ray_direction :: Vector
} deriving (Show)


data Camera = Camera {
    camera_position  :: Point,
    camera_direction :: Vector,
    camera_canvas_width :: Float,
    camera_canvas_height :: Float,
    camera_canvas_distance :: Float,
    camera_resolution :: Float -- 1/pixel
} deriving (Show)


-- Represents a scene (list of objects)
data Scene = Scene {
    scene_objects :: [Object], -- List of objects
    scene_ambient :: Color,    -- Ambient light
    scene_background :: Color -- Background color
} deriving (Show)


-- Returns a vector that represents the direction from
-- one given point to the other
vector :: Point -> Point -> Vector
vector (Point x1 y1 z1) (Point x2 y2 z2) = Vector (x2-x1) (y2-y1) (z2-z1)


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
closestPoint p points = foldl1' (\p1 p2 -> if distance p2 p < distance p1 p then p2 else p1) points


-- Returns the closest object to a ray base in the direction of the ray
-- Todo: we should work with shapes here, not objects
firstHit :: Ray -> [Object] -> Maybe Incidence
firstHit r@(Ray p d) objects = foldl' (closerIncidence r) Nothing objects
    where
        closerIncidence :: Ray -> Maybe Incidence -> Object -> Maybe Incidence
        closerIncidence r@(Ray p d) i1 o2
            | isNothing i1 = i2
            | isNothing i2 = i1
            | d1 < d2      = i1
            | otherwise    = i2
            where
                i2 = incidence r o2
                d1 = distance p (incidence_point $ fromJust i1)
                d2 = distance p (incidence_point $ fromJust i2)


-- Calculates the intersection of a ray and a shape.
-- Returns the closest intersection to the starting
-- point of the ray.
incidence :: Ray -> Object -> Maybe Incidence

-- Incidence with a sphere
-- We move the ray and sphere so that the sphere's center is
-- at (0,0,0). This way the quadratic equation is simpler.
incidence
    (Ray (Point rx ry rz) (Vector rdx rdy rdz))
    obj@(Object Sphere _) =

    if isNothing point
    then Nothing
    else Just $ Incidence obj (fromJust point) (Vector px py py)
    where
        (Point px py pz) = fromJust point
        point =
            if solutions == []
            then Nothing
            else
                let x = minimum solutions
                in Just $ Point (x*rdx+rx) (x*rdy+ry) (x*rdz+rz)
        solutions = filter (> 0) $ solveQuadratic
            (rdx**2+rdy**2+rdz**2)
            (2*rx*rdx+2*ry*rdy+2*rz*rdz)
            (rx**2+ry**2+rz**2-1)

-- Intersection with a cube
incidence
    (Ray (Point rx ry rz) (Vector rdx rdy rdz))
    obj@(Object Cube _) =

    Nothing


-- Tell the color seen by a single ray
renderPixel :: Ray -> Scene -> Color

renderPixel ray (Scene objs amb bg)
    | isNothing i = bg
    | otherwise   = let Just (Incidence (Object _ (Finish c _ _ _ _ _)) _ _) = i in c
    where
        i = firstHit ray objs


render :: Scene -> Camera -> [(Point,Color)]

render scene cam@(Camera cam_p cam_dir cam_w cam_h cam_dist cam_r) =
    [
        (p, renderPixel (Ray cam_p (vector cam_p p)) scene) |
        p <- [Point x y (vector_z cam_dir) | x <- [-10,-9.98..10], y <- [-10,-9.98..10]] -- Always looks at 0,0,cam_dir
    ]


main :: IO ()

main = do
    print $ render
        (Scene
            [Object
                Sphere
                (Finish (Color 0 0.7 0) (Color 0 0 0) (Color 0 0 0) 0 0 0)] -- Green sphere
            (Color 0.7 0.7 0.7) -- Ambient white light
            (Color 0.3 0.3 0.8) -- Background is light green
        )
        (Camera
            (Point 0 0 0)
            (Vector 0 0 1)
            20 20 5 1
        )
