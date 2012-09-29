import Data.List
import Data.Maybe
import qualified Graphics.GD as GD

import Color
import Shape
import Vector

data Finish = Finish
    !Color -- Ambient
    !Color -- Diffuse
    !Double -- Reflect. TODO: add tint
    !Double -- 0: opaque, 1: transparent
    !Double -- 0: diffuse, 1: shiny
    !Double -- 1: air
    deriving (Show)

data Object = Object !Shape !Finish !Transformation deriving (Show)

data Light
    = Spot !Vector !Color
    deriving (Show)

-- Camera is always at (0,0,0)
-- and the canvas center is always at (0,0,1)
data Camera = Camera
    !Double -- Canvas width
    !Double -- Canvas height
    !Double -- resolution 1/pixel
    !Double -- Camera distance from canvas
    deriving (Show)

-- Represents a scene (list of objects)
data Scene = Scene
    [Object] -- List of objects
    !Light    -- Light, just one for now
    !Color    -- Ambient light
    !Color    -- Background color
    deriving (Show)

-- Takes transformation into account
incidence' :: Ray -> Object -> Maybe Incidence
incidence' (Ray rv d) (Object s _ t)
    | isNothing i = Nothing
    | otherwise   = Just $ Incidence (normalize $ trans t iv) (normalize $ itrans (stripTrans t) n)
    where
        i = incidence (Ray (normalize $ itrans t rv) (normalize $ itrans (stripTrans t) d)) s
        Just (Incidence iv n) = i

incidences :: Ray -> [Object] -> [(Object, Incidence)]
incidences ray objs
    | null objs    = []
    | isNothing mi = rest
    | otherwise    = (o, i) : rest
    where
        o = head objs
        rest = incidences ray $ tail objs
        mi = incidence' ray o
        Just i = mi

-- Returns the closest object to a ray base in the direction of the ray
-- Todo: we should work with shapes here, not objects
-- This takes care of the object transformations
firstHit :: Ray -> [Object] -> Maybe (Object, Incidence)
firstHit ray@(Ray v _) objs
    | null incs = Nothing
    | otherwise = Just $ minimumBy (\(_,Incidence v1 _) (_,Incidence v2 _) -> compare (vlen $ sub v1 v) (vlen $ sub v2 v)) incs
    where
        incs = incidences ray objs

-- Tells wether there is an obstacle between two points
isObstructed :: Vector -> Vector -> [Object] -> Bool
isObstructed v1 v2 objs =
    any
        (\(_, Incidence v _) -> (vlen $ sub v1 v) < (vlen $ sub v1 v2))
        (incidences (Ray v1 (sub v2 v1)) objs)

-- Tell the color seen by a single ray
colorSeenBy :: Ray -> Scene -> Color

colorSeenBy ray@(Ray _ rd) s@(Scene objs (Spot lv (Color lr lg lb)) _ bg)
    | isNothing i = bg
    | isObstructed i_vect lv objs = Color (ar+rr*refl) (ag+rb*refl) (ab+rb*refl)
    | otherwise = Color (ar+dr*lr*lint+(rr*refl)) (ag+dg*lg*lint+(rg*refl)) (ab+db*lb*lint+(rb*refl)) -- FUJJJJJJ
    where
        i = firstHit ray objs
        Just ((Object _ (Finish (Color ar ag ab) (Color dr dg db) refl _ _ _) _), Incidence i_vect i_norm) = i
        lint = max 0 $ vcosphi lv i_norm
        Color rr rg rb =
            if refl <= 0
            then Color 0 0 0
            else colorSeenBy (Ray i_vect $ reflection rd i_norm) s

-- Returns coordinates on the image, and the rays through those
-- coordinates
rays :: Camera -> [((Int,Int),Ray)]
rays (Camera w h r d) =
    [
        ((x, y), Ray (Vector 0 0 (-d) 1) (Vector (-w/2+fromIntegral(x)/r) (-h/2+fromIntegral(y)/r) d 1) )
        | x <- [0..round(w*r)], y <- [0..round(h*r)]
    ]

render :: Scene -> Camera -> [((Int, Int),Color)]

render scene cam = [((x, y), colorSeenBy ray scene) | ((x, y), ray) <- rays cam]

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
