import Data.List
import Data.Maybe
import qualified Graphics.GD as GD

import Color
import Shape
import Vector

data Finish = Finish
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

-- Blinn-Phong shading
-- View direction, light direction, spec. color, surface normal.
blinnPhong :: Vector -> Vector -> Color -> Vector -> Color
blinnPhong vd ld c normal = 
    cscale blinnTerm2 c
  where
    blinnDir = sub ld vd
    temp = sqrt(dotp blinnDir blinnDir)
    blinnDir2 = scale (1/temp) blinnDir
    blinnTerm = max (dotp blinnDir2 normal) 0
    specValue = 2 -- TODO: material param
    coeff = 1     -- TODO: param
    specPower = 30 -- TODO: material param
    blinnTerm2 = specValue * blinnTerm ** specPower * coeff -- TODO: specular value instead of 1, coeff instead of 2

-- Tell the color seen by a single ray
colorSeenBy :: Ray -> Scene -> Color

colorSeenBy ray@(Ray _ rd) s@(Scene objs (Spot lv lightc) _ bg)
    | isNothing i = bg
    | isObstructed i_vect lv objs = cadd amb $ cfilter refl tint
    | otherwise = cadd (cfilter diff $ cscale lint lightc) $ cadd (cfilter refl tint) $ cadd amb $ cscale lint $ cfilter lightc $ blinnPhong rd (normalize $ sub lv i_vect) diff i_norm -- TODO: normalize directions
    -- | otherwise = cadd (cadd amb (cfilter diff $ cscale lint lightc)) (cfilter refl tint)
    where
        i = firstHit ray objs
        Just ((Object _ (Finish amb diff tint _ _ _) _), Incidence i_vect i_norm) = i
        lint = (max 0 $ vcosphi lv i_norm) / (vlen $ sub lv i_vect) ** 2
        refl =
            if isIntense 0.01 tint -- TODO: add color treshold as parameter
            then colorSeenBy (Ray i_vect $ reflection rd i_norm) s
            else Color 0 0 0

-- Returns coordinates on the image, and the rays through those
-- coordinates
rays :: Camera -> [((Int,Int),Ray)]
rays (Camera w h r d) =
    [
        ((x, y), Ray (Vector 0 0 (-d) 1) (normalize $ Vector (-w/2+fromIntegral(x)/r) (-h/2+fromIntegral(y)/r) d 1) )
        | x <- [0..round(w*r)], y <- [0..round(h*r)]
    ]

render :: Scene -> Camera -> [((Int, Int),Color)]

render scene cam = [((x, y), colorSeenBy ray scene) | ((x, y), ray) <- rays cam]

-- Shift colors between 0 and 1 using exponential function
expose :: Double -> Double
expose c = 1 - (3 ** (-c))

-- Execute main program.
main :: IO ()

-- Get the list of coordinates and associated colors and call GD's
-- setPixel function on each of them using a freshly created image.
main = do
    image <- GD.newImage (1001, 1001)
    mapM_
        (\((x, y), (Color r g b)) -> do
            GD.setPixel (y, 1000-x) (GD.rgb (round (255 * expose r)) (round (255 * expose g)) (round (255 * expose b))) image -- TODO: better exposure calculation
            --print (x, y)
        )
        (render
            (Scene
                [
                    Object Sphere (Finish (Color 0 0.2 0) (Color 0.4 0.7 0.4) (Color 0.4 0.8 0.4) 0 0 0) (translate 1 0 1.1),
                    Object Sphere (Finish (Color 0 0 0.2) (Color 0.4 0.4 0.7) (Color 0.4 0.4 0.8) 0 0 0) (translate (-1) 0 1.5),
                    Object Sphere (Finish (Color 0.2 0 0) (Color 0.7 0.4 0.4) (Color 0.8 0.4 0.4) 0 0 0) (translate 0 (-3) 6),
                    Object Sphere (Finish (Color 0.2 0 0.2) (Color 0.7 0.4 0.7) (Color 0.8 0.4 0.8) 0 0 0) (translate (3) (3) 2),
                    Object Sphere (Finish (Color 0 0.2 0.2) (Color 0.4 0.7 0.7) (Color 0.4 0.8 0.8) 0 0 0) (translate (-3) (3) 2)
                ]
                (Spot (Vector 30 0 (-10) 1) (Color 1000 1000 1000))
                (Color 1 1 1)
                (Color 0.2 0.2 0.2)
            )
            (Camera 10 10 100 10)
        )
    GD.savePngFile "raytracer.png" image
