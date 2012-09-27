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

data Finish = Finish
    Color -- Ambient
    Color -- Diffuse
    Color -- Reflect
    Float -- 0: opaque, 1: transparent
    Float -- 0: diffuse, 1: shiny
    Float -- 1: air
    deriving (Show)

data Object = Object Shape Finish Transformation deriving (Show)

data Light
    = Spot Vector Color
    deriving (Show)

-- When a ray hit something, it expressed
-- with an Incidence object
data Incidence = Incidence
    Object -- The object
    Vector -- The point on the object's shape
    Vector -- The normal at that point
    deriving (Show)

-- Represents a ray (semi line)
data Ray = Ray {
    ray_origin :: Vector,
    ray_direction :: Vector
} deriving (Show)

-- Camera is always at (0,0,0)
-- and the canvas center is always at (0,0,1)
data Camera = Camera
    Float -- Canvas width
    Float -- Canvas height
    Float -- resolution 1/pixel
    Float -- Camera distance from canvas
    deriving (Show)

-- Represents a scene (list of objects)
data Scene = Scene
    [Object] -- List of objects
    Light    -- Light, just one for now
    Color    -- Ambient light
    Color    -- Background color
    deriving (Show)

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
        closerIncidence r@(Ray v _) i1 o2@(Object _ _ t2)
            | isNothing i2 = i1
            | isNothing i1 = i2t
            | d1 < d2      = i1
            | otherwise    = i2t
            where
                i2 = incidence (Ray (normalize $ itrans t2 (ray_origin r)) (normalize $ itrans (stripTrans t2) (ray_direction r))) o2
                (Just (Incidence i2o i2v i2n)) = i2
                i2t = Just $ Incidence i2o (trans t2 i2v) i2n -- TODO: incidence as a functor?
                d1 = let Incidence _ i1v _ = fromJust i1 in vlen $ sub v i1v
                d2 = let Incidence _ i2tv _ = fromJust i2t in vlen $ sub v i2tv -- TODO: heavy refactor needed

-- Calculates the intersection of a ray and a shape.
-- Returns the closest intersection to the starting
-- Vector of the ray.
incidence :: Ray -> Object -> Maybe Incidence

-- Incidence with a unit sphere at origo
incidence (Ray (Vector rx ry rz _) (Vector rdx rdy rdz _)) obj@(Object Sphere _ _) =
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
        solutions = filter (> 0.00001) $ solveQuadratic
            (rdx**2+rdy**2+rdz**2)
            (2*rx*rdx+2*ry*rdy+2*rz*rdz)
            (rx**2+ry**2+rz**2-1)

-- Intersection with a cube
incidence _ (Object Cube _ _) = Nothing

-- Tells wether there is an obstacle between two points
isObstructed :: Vector -> Vector -> [Object] -> Bool
isObstructed v1 v2 objs = isJust $ firstHit (Ray v1 (sub v2 v1)) objs

-- Tell the color seen by a single ray
renderPixel :: Ray -> Scene -> Color

renderPixel ray (Scene objs (Spot lv (Color lr lg lb)) _ bg)
    | isNothing i = bg
    | isObstructed i_vect lv objs = c -- TODO: better obstruction detection from a surface
    | otherwise = Color (ar+dr*lr*lint) (ag+dg*lg*lint) (ab+db*lb*lint) -- FUJJJJJJ
    where
        i = firstHit ray objs
        Just (Incidence (Object _ (Finish c@(Color ar ag ab) (Color dr dg db) _ _ _ _) _) _ _) = i
        Just (Incidence _ i_vect i_norm) = i
        lint = max 0 $ vcosphi lv i_norm

-- Returns coordinates on the image, and the rays through those
-- coordinates
rays :: Camera -> [((Int,Int),Ray)]
rays (Camera w h r d) =
    [
        ((x, y), Ray (Vector 0 0 (-d) 1) (Vector (-w/2+fromIntegral(x)/r) (-h/2+fromIntegral(y)/r) d 1) )
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
                    Object Sphere (Finish (Color 0 0.1 0) (Color 0.2 0.7 0.2) (Color 0 0 0) 0 0 0) (translate 1 0 1.1),
                    Object Sphere (Finish (Color 0 0 0.1) (Color 0.2 0.2 0.7) (Color 0 0 0) 0 0 0) (translate (-1) 0 1.5),
                    Object Sphere (Finish (Color 0 0 0.1) (Color 0.7 0.2 0.2) (Color 0 0 0) 0 0 0) (translate (-6) (-6) 10),
                    Object Sphere (Finish (Color 0 0 0.1) (Color 0.7 0.2 0.7) (Color 0 0 0) 0 0 0) (translate (6) (6) 3),
                    Object Sphere (Finish (Color 0 0 0.1) (Color 0.2 0.7 0.7) (Color 0 0 0) 0 0 0) (translate (-6) (6) 3)
                ]
                (Spot (Vector 30 0 (-10) 1) (Color 1 1 1))
                (Color 0.7 0.7 0.7)
                (Color 0.1 0.1 0.1)
            )
            (Camera 10 10 50 10)
        )
    GD.savePngFile "raytracer.png" image
