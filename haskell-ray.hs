import Data.List
import Data.Maybe
import qualified Graphics.GD as GD

import Vectorgeo

-- Color in R G B format
-- values ranges from 0 to 1
data Color = Color Double Double Double deriving (Show)

data Shape =
    Cube  -- Unit cube 
    | Sphere -- Unit sphere
    deriving (Show)

data Finish = Finish
    Color -- Ambient
    Color -- Diffuse
    Double -- Reflect. TODO: add tint
    Double -- 0: opaque, 1: transparent
    Double -- 0: diffuse, 1: shiny
    Double -- 1: air
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
    Double -- Canvas width
    Double -- Canvas height
    Double -- resolution 1/pixel
    Double -- Camera distance from canvas
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
solveQuadratic :: Double -> Double -> Double -> [Double]
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
                i2t = Just $ Incidence i2o (normalize $ trans t2 i2v) (normalize $ trans (stripTrans t2) i2n) -- TODO: incidence as a functor?
                d1 = let Incidence _ i1v _ = fromJust i1 in vlen $ sub v i1v
                d2 = let Incidence _ i2tv _ = fromJust i2t in vlen $ sub v i2tv -- TODO: heavy refactor needed

-- Calculates the intersection of a ray and a shape.
-- Returns the closest intersection to the starting
-- Vector of the ray.
-- Works on unit shapes! You must take care of the appropriate
-- transformations for this to be useful!
incidence :: Ray -> Object -> Maybe Incidence

-- Incidence with a unit sphere at origo
incidence (Ray (Vector rx ry rz _) (Vector rdx rdy rdz _)) obj@(Object Sphere _ _) =
    if isNothing maybe_vector
    then Nothing
    else Just $ Incidence obj vector vector -- Yepp, it's a unit sphere.
    where
        vector = fromJust maybe_vector
        maybe_vector =
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

renderPixel ray@(Ray _ rd) s@(Scene objs (Spot lv (Color lr lg lb)) _ bg)
    | isNothing i = bg
    | isObstructed i_vect lv objs = Color (ar+rr*refl) (ag+rb*refl) (ab+rb*refl) -- TODO: better obstruction detection from a surface
    | otherwise = Color (ar+dr*lr*lint+(rr*refl)) (ag+dg*lg*lint+(rg*refl)) (ab+db*lb*lint+(rb*refl)) -- FUJJJJJJ
    where
        i = firstHit ray objs
        Just (Incidence (Object _ (Finish (Color ar ag ab) (Color dr dg db) refl _ _ _) _) i_vect i_norm) = i
        lint = max 0 $ vcosphi lv i_norm
        Color rr rg rb =
            if refl <= 0
            then Color 0 0 0
            else renderPixel (Ray i_vect $ reflection rd i_norm) s

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
    image <- GD.newImage (1001, 1001)
    mapM_
        (\((x, y), (Color r g b)) -> do
            GD.setPixel (y, 1000-x) (GD.rgb (round (r*256)) (round (g*256)) (round (b*256))) image
            --print (x, y)
        )
        (render
            (Scene
                [
                    Object Sphere (Finish (Color 0 0.1 0) (Color 0.2 0.7 0.2) 0.7 0 0 0) (translate 1 0 1.1),
                    Object Sphere (Finish (Color 0 0 0.1) (Color 0.2 0.2 0.7) 0.7 0 0 0) (translate (-1) 0 1.5),
                    Object Sphere (Finish (Color 0.1 0 0) (Color 0.7 0.2 0.2) 0.7 0 0 0) (translate 0 (-3) 6),
                    Object Sphere (Finish (Color 0.1 0 0.1) (Color 0.7 0.2 0.7) 0.7 0 0 0) (translate (3) (3) 2),
                    Object Sphere (Finish (Color 0 0.1 0.1) (Color 0.2 0.7 0.7) 0.7 0 0 0) (translate (-3) (3) 2)
                ]
                (Spot (Vector 30 0 (-10) 1) (Color 1 1 1))
                (Color 0.7 0.7 0.7)
                (Color 0.1 0.1 0.1)
            )
            (Camera 10 10 100 10)
        )
    GD.savePngFile "raytracer.png" image
