import Data.List
import Data.Maybe
import Control.Concurrent.ParallelIO.Global
import qualified Graphics.GD as GD

import Color
import Shape
import Vector

-- TODO
-- Transparency
-- Refraction
-- Shapes: Cube, Cone, Torus, Plane, Membrane (square)
-- Procedural textures: checker, turbulent textures, etc.
-- Random bump map, depth of field
-- CSG: union, intersection, difference, blobbing union and intersection
-- Shapes: triangle, circle, polygon lines, bezier curves, prism, lathe
-- Lights: directional, area
-- Global illumination: photon map

data Finish = Finish -- TODO: specular color, parameters
    !Color -- Ambient
    !Color -- Diffuse
    !Color -- Reflection tint
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
    | otherwise   = Just $ Incidence (correct $ trans t iv) (correct $ itrans (stripTrans t) n)
    where
        i = incidence (Ray (correct $ itrans t rv) (correct $ itrans (stripTrans t) d)) s
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
firstHit :: Ray -> [Object] -> Maybe (Object, Incidence) -- TODO: no tuples please.
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

-- Blinn-Phong shading
-- View direction, light direction, spec. color, surface normal.
blinnPhong :: Vector -> Vector -> Vector -> Color -> Color
blinnPhong vd ld normal c = 
    cscale blinnTerm2 c
  where
    blinnDir = sub ld vd
    temp = sqrt(dotp blinnDir blinnDir)
    blinnDir2 = scale (1/temp) blinnDir
    blinnTerm = max (dotp blinnDir2 normal) 0
    specValue = 5 -- TODO: material param
    coeff = 1     -- TODO: param
    specPower = 200 -- TODO: material param
    blinnTerm2 = specValue * blinnTerm ** specPower * coeff -- TODO: specular value instead of 1, coeff instead of 2

-- Diffuse lighting
-- light direction, diffuse color, surface normal
diffuse :: Vector -> Vector -> Color -> Color
diffuse ld normal c = cscale (max 0 $ vcosphi ld normal) c

-- Tell the color seen by a single ray
colorSeenBy :: Ray -> Scene -> Color

colorSeenBy ray@(Ray _ rd) s@(Scene objs (Spot lv lightc) _ bg) -- TODO: refactor the SHIT out of these long lines
    | isNothing i = bg
    | isObstructed i_vect lv objs = cadd amb $ cfilter refl tint
    | otherwise = cadd amb $ cadd diff $ cadd spec $ cadd refl refr
  where
    i = firstHit ray objs
    Just ((Object _ (Finish amb diffc tint _ _ _) _), Incidence i_vect i_norm) = i
    lint = 1 / (vlen $ sub lv i_vect) ** 2 -- Light intensity depending on distance
    refl = -- Reflected ray
        if isIntense 0.01 tint
        then colorSeenBy (Ray i_vect $ reflection rd i_norm) s
        else Color 0 0 0 -- TODO: add color treshold as parameter
    diff = cfilter lightc $ cscale lint $ diffuse (normalize $ sub lv i_vect) i_norm diffc -- Diffuse component
    spec = cfilter lightc $ cscale lint $ blinnPhong rd (normalize $ sub lv i_vect) i_norm diffc -- Specular component. TODO: specular color
    refr = Color 0 0 0 -- Refracted component. TODO.

-- Returns coordinates on the image, and the rays through those
-- coordinates
-- rays :: Camera -> [((Int,Int),Ray)]
-- rays (Camera w h r d) =
--     [((x, y), Ray (Vector 0 0 (-d) 1) (normalize $ Vector (-w/2+fromIntegral(x)/r) (-h/2+fromIntegral(y)/r) d 1) )
--     | x <- [0..round(w*r)-1], y <- [0..round(h*r)-1]]

-- Shift colors between 0 and 1 using exponential function
expose :: Double -> Double
expose c = 1 - (2.7182 ** (-c))

renderRow :: GD.Image -> Int -> Scene -> Camera -> IO ()
renderRow image y scene (Camera w h r d) = do
    mapM_
        (\(x, y, Color r g b) -> GD.setPixel (y, 1000-x) (GD.rgb (round (255 * expose r)) (round (255 * expose g)) (round (255 * expose b))) image)
        [(x, y, colorSeenBy (Ray (Vector 0 0 (-d) 1) (normalize $ Vector (-w/2+fromIntegral(x)/r) (-h/2+fromIntegral(y)/r) d 1)) scene) | x <- [0..round(w*r)]]

-- render :: Scene -> Camera -> [((Int, Int),Color)]
-- render scene cam = map (\((x, y), ray) -> ((x, y), colorSeenBy ray scene)) (rays cam)

render :: Scene -> Camera -> IO GD.Image
render scene cam@(Camera w h r _) = do
    image <- GD.newImage (width, height)
    parallel_ [renderRow image y scene cam | y <- [0..height]]
    return image
  where
    width = round(w*r)
    height = round(h*r)

-- Execute main program.
main :: IO ()
main = do
    image <- render
        (Scene
            [
                Object Sphere (Finish (Color 0.1 0.3 0.1) (Color 0.3 0.4 0.3) (Color 0.7 0.9 0.7) 0 0 0) (translate 1 0 1.1),
                Object Sphere (Finish (Color 0.1 0.1 0.3) (Color 0.3 0.3 0.4) (Color 0.7 0.7 0.9) 0 0 0) (translate (-1) 0 1.5),
                Object Sphere (Finish (Color 0.3 0.1 0.1) (Color 0.4 0.3 0.3) (Color 0.9 0.7 0.7) 0 0 0) (translate 0 (-3) 6),
                Object Sphere (Finish (Color 0.3 0.1 0.3) (Color 0.4 0.3 0.4) (Color 0.9 0.7 0.9) 0 0 0) (translate (3) (3) 2),
                Object Sphere (Finish (Color 0.1 0.3 0.3) (Color 0.3 0.4 0.4) (Color 0.7 0.9 0.9) 0 0 0) (translate (-3) (3) 2)
            ]
            (Spot (Vector 90 0 (-30) 1) (Color 9000 9000 9000))
            (Color 1 1 1)
            (Color 0 0 0)
        )
        (Camera 10 10 100 10)
    stopGlobalPool
    GD.savePngFile "raytracer.png" image
