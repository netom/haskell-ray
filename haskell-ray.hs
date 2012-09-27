import Data.List
import Data.Maybe
import qualified Graphics.GD as GD

import Raygeo

-- Color in R G B format
-- values ranges from 0 to 1
data Color = Color Float Float Float deriving (Show)


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


data Light =
    Spot {
        light_position :: Vector,
        light_color :: Color
    }
    deriving (Show)


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
    scene_light :: Light,      -- Light
    scene_ambient :: Color,    -- Ambient light
    scene_background :: Color  -- Background color
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
                t2 = object_trans o2
                i2 = incidence (Ray (normalize $ trans t2 (ray_origin r)) (normalize $ trans (stripTrans t2) (ray_direction r))) o2
                --i2t = Just $ Incidence i2o (itrans t2 i2v) i2n -- TODO: itt mátrixot kellene invertálni. szopó.
                d1 = vlen $ sub v (incidence_vector $ fromJust i1)
                d2 = vlen $ sub v (incidence_vector $ fromJust i2)


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
        solutions = filter (> 0.0000001) $ solveQuadratic
            (rdx**2+rdy**2+rdz**2)
            (2*rx*rdx+2*ry*rdy+2*rz*rdz)
            (rx**2+ry**2+rz**2-1)

-- Intersection with a cube
incidence
    (Ray (Vector rx ry rz _) (Vector rdx rdy rdz _))
    (Object Cube _ _) =

    Nothing

-- Tells wether there is an obstacle between two points
isObstructed :: Vector -> Vector -> [Object] -> Bool
isObstructed v1 v2 objs = isJust $ firstHit (Ray v1 (sub v2 v1)) objs

-- Tell the color seen by a single ray
renderPixel :: Ray -> Scene -> Color

renderPixel ray (Scene objs (Spot lv (Color lr lg lb)) amb bg)
    | isNothing i = bg
    | isObstructed i_vect lv objs = c -- TODO: better obstruction detection from a surface
    | otherwise = Color (max (dr*lr*lint) ar) (max (dg*lg*lint) ag) (max (db*lb*lint) ab) -- FUJJJJJJ
    where
        i = firstHit ray objs
        Just (Incidence (Object _ (Finish c@(Color ar ag ab) (Color dr dg db) _ _ _ _) _) _ _) = i
        Just (Incidence i_obj i_vect i_norm) = i
        lint = vcosphi lv i_norm

-- Returns coordinates on the image, and the rays through those
-- coordinates
rays :: Camera -> [((Int,Int),Ray)]
rays (Camera w h r) =
    [
        ((x, y), Ray (Vector (-w/2+fromIntegral(x)/r) (-h/2+fromIntegral(y)/r) 1 1) (Vector 0 0 1 1) )
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
        (\((x, y), (Color r g b)) -> GD.setPixel (y, 499-x) (GD.rgb (round (r*256)) (round (g*256)) (round (b*256))) image)
        (render
            (Scene
                [
                    Object Sphere (Finish (Color 0 0.1 0) (Color 0.2 0.7 0.2) (Color 0 0 0) 0 0 0) (translate 1 0 2),
                    Object Sphere (Finish (Color 0 0 0.1) (Color 0.2 0.2 0.7) (Color 0 0 0) 0 0 0) (translate (-1) 0 2)
                ]
                (Spot (Vector 30 0 0 1) (Color 1 1 1))
                (Color 0.7 0.7 0.7)
                (Color 0.1 0.1 0.1)
            )
            (Camera 10 10 50)
        )
    GD.savePngFile "raytracer.png" image
