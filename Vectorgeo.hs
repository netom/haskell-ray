module Vectorgeo (
    Transformation(..),
    stay, translate, stripTrans,
    Vector(..),
    normalize, vcosphi, origo,
    vlen, closestVector,
    trans, itrans, dotp, add, sub, scale,
    reflection
) where

import Data.List

data Transformation = Transformation {
    a11  :: Double, a12  :: Double, a13  :: Double, a14  :: Double,
    a21  :: Double, a22  :: Double, a23  :: Double, a24  :: Double,
    a31  :: Double, a32  :: Double, a33  :: Double, a34  :: Double,
    a41  :: Double, a42  :: Double, a43  :: Double, a44  :: Double,

    ia11 :: Double, ia12 :: Double, ia13 :: Double, ia14 :: Double,
    ia21 :: Double, ia22 :: Double, ia23 :: Double, ia24 :: Double,
    ia31 :: Double, ia32 :: Double, ia33 :: Double, ia34 :: Double,
    ia41 :: Double, ia42 :: Double, ia43 :: Double, ia44 :: Double
} deriving (Show)

stay :: Transformation
stay = Transformation
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1

    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1

translate :: Double -> Double -> Double -> Transformation
translate x y z = Transformation
    1 0 0 x
    0 1 0 y
    0 0 1 z
    0 0 0 1

    1 0 0 (-x)
    0 1 0 (-y)
    0 0 1 (-z)
    0 0 0 1

stripTrans :: Transformation -> Transformation
stripTrans t = Transformation
    (a11 t)  (a12 t)  (a12 t)  0
    (a21 t)  (a22 t)  (a23 t)  0
    (a31 t)  (a32 t)  (a33 t)  0
    0        0        0        1

    (ia11 t) (ia12 t) (ia12 t) 0
    (ia21 t) (ia22 t) (ia23 t) 0
    (ia31 t) (ia32 t) (ia33 t) 0
    0        0        0        1

-- Vector with homogeneous coordinates
-- x, y, z, h
data Vector = Vector Double Double Double Double deriving (Show)

origo :: Vector
origo = Vector 0 0 0 1

trans :: Transformation -> Vector -> Vector
trans t (Vector x y z h) = Vector
    ((a11 t)*x+(a12 t)*y+(a13 t)*z+(a14 t)*h)
    ((a21 t)*x+(a22 t)*y+(a23 t)*z+(a24 t)*h)
    ((a31 t)*x+(a32 t)*y+(a33 t)*z+(a34 t)*h)
    ((a41 t)*x+(a42 t)*y+(a43 t)*z+(a44 t)*h)

itrans :: Transformation -> Vector -> Vector
itrans t (Vector x y z h) = Vector
    ((ia11 t)*x+(ia12 t)*y+(ia13 t)*z+(ia14 t)*h)
    ((ia21 t)*x+(ia22 t)*y+(ia23 t)*z+(ia24 t)*h)
    ((ia31 t)*x+(ia32 t)*y+(ia33 t)*z+(ia34 t)*h)
    ((ia41 t)*x+(ia42 t)*y+(ia43 t)*z+(ia44 t)*h)

dotp :: Vector -> Vector -> Double
dotp (Vector x1 y1 z1 _) (Vector x2 y2 z2 _) = x1*x2+y1*y2+z1*z2

vcosphi :: Vector -> Vector -> Double
vcosphi v1 v2 = dotp v1 v2 / (vlen v1 * vlen v2)

add :: Vector -> Vector -> Vector
add (Vector x1 y1 z1 _) (Vector x2 y2 z2 _) = Vector (x1+x2) (y1+y2) (z1+z2) 1

sub :: Vector -> Vector -> Vector
sub (Vector x1 y1 z1 _) (Vector x2 y2 z2 _) = Vector (x1-x2) (y1-y2) (z1-z2) 1

normalize :: Vector -> Vector
normalize (Vector x y z h) = Vector (x/h) (y/h) (z/h) 1

scale :: Double -> Vector -> Vector
scale s (Vector x y z h) = Vector (s*x) (s*y) (s*z) h

-- Calculates the length of a vector
-- does not care about the h coordinate, it must be 1!
vlen :: Vector -> Double
vlen (Vector x y z _) = sqrt(x**2+y**2+z**2)

-- Returns the closest point to a given point from a point list
closestVector :: Vector -> [Vector] -> Vector
closestVector v vectors = foldl1' (\v1 v2 -> if vlen (sub v2 v) < vlen (sub v1 v) then v2 else v1) vectors

-- Given the incident and normal vector returns
-- a vector that represents the direction of a reflected ray
reflection :: Vector -> Vector -> Vector
reflection incident normal = sub incident $ scale (2 * dotp incident normal) normal
