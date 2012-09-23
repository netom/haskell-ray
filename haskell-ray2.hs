-- Color in R G B format
-- values ranges from 0 to 1
data Color = Color {
    color_r :: Float,
    color_g :: Float,
    color_b :: Float
} deriving (Show)

data Point = Point {
    point_x :: Float,
    point_y :: Float,
    point_z :: Float
} deriving (Show)

data Vector = Vector {
    vector_x :: Float,
    vector_y :: Float,
    vector_z :: Float
} deriving (Show)

data Shape =
    Box {
        box_corner1 :: Point,
        box_corner2 :: Point
    }
    | Sphere {
        sphere_center :: Point,
        spehre_radius :: Float
    }
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
    camera_canves_distance :: Float,
    camera_resolution :: Float -- 1/pixel
} deriving (Show)

-- Represents a scene (list of objects)
data Scene = Scene {
    scene_objects :: [Object] -- List of objects
    scene_ambient :: Color    -- Ambient light
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
incidence :: Ray -> Object -> Maybe Incidence

-- Intersection with a sphere
-- We move the ray and sphere so that the sphere's center is
-- at (0,0,0). This way the quadratic equation is simpler.
incidence
    (Ray (Point rx ry rz) (Vector rdx rdy rdz))
    obj@(Object (Sphere (Point sx sy sz) r) _) =

    if point == Nothing
    then Nothing
    else Just $ Incidence obj point (Vector (px-sx) (py-sy) (py-sz))
    where
        (tx, ty, tz) = (rx-sx, ry-sy, rz-sz) -- Translated ray origin
        (Point px py pz) = point
        point =
            if solutions == []
            then Nothing
            else
                let x = minimum solutions
                in Just $ Point (x*rdx+rx) (x*rdy+ry) (x*rdz+rz)
        solutions = filter (> 0) $ solveQuadratic
            (rdx**2+rdy**2+rdz**2)
            (2*tx*rdx+2*ty*rdy+2*tz*rdz)
            (tx**2+ty**2+tz**2-r**2)

-- Intersection with a cube
incidence
    (Ray (Point rx ry rz) (Vector rdx rdy rdz))
    (Box (Point p1x p1y p1z) (Point p2x p2y p2z)) =

    Nothing


renderPixel :: Ray -> Scene -> Color

renderPixel ray (Scene objs amb bg)
    | int == Nothing = bg
    | otherwise   = let Just (Object _ (Material _ _ c _ _)) = int in c
    where
        int = firstHit ray objs


render :: Scene -> Camera -> [(Point,Color)]

render scene cam =
    where
    rays = map
        (\p -> Ray (camera_position cam) $ vector (camera_position cam) p)
        [Point ()]

main = do
    print $ render
        (Scene
            [Object
                (Sphere (Point 0 0 10) 2)
                (Material 0 0 (Color 0 0.7 0) (Color 0 0 0) (Color 0 0 0))] -- Green sphere
            (Color 0.7 0.7 0.7) -- Ambient white light
            (Color 0.3 0.3 0.8) -- Background is light green
        )
        (Camera
            (Point 0 0 0)
            (Vector 0 0 1)
            20 20 5 1
        )