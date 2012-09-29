module Shape (
    Shape(..), Incidence(..),
    incidence
) where

import Vector
import Data.List
import Data.Maybe

-- When a ray hit something, it expressed
-- with an Incidence object
data Incidence = Incidence
    !Vector -- The point on the object's shape
    !Vector -- The normal at that point
    deriving (Show)

data Shape =
    Cube  -- Unit cube 
    | Sphere -- Unit sphere
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

-- Calculates the intersection of a ray and a shape.
-- Returns the closest intersection to the starting
-- Vector of the ray.
-- Works on unit shapes! You must take care of the appropriate
-- transformations for this to be useful!
incidence :: Ray -> Shape -> Maybe Incidence

-- Incidence with a unit sphere at origo
incidence (Ray (Vector rx ry rz _) (Vector rdx rdy rdz _)) Sphere =
    if isNothing maybe_vector
    then Nothing
    else Just $ Incidence vector vector -- Yepp, it's a unit sphere.
    where
        vector = fromJust maybe_vector
        maybe_vector =
            if solutions == []
            then Nothing
            else
                let x = minimum solutions
                in Just $ Vector (x*rdx+rx) (x*rdy+ry) (x*rdz+rz) 1
        solutions = Data.List.filter (> 0.0000001) $ solveQuadratic
            (rdx**2+rdy**2+rdz**2)
            (2*rx*rdx+2*ry*rdy+2*rz*rdz)
            (rx**2+ry**2+rz**2-1)

-- Intersection with a cube
incidence _ Cube = Nothing
