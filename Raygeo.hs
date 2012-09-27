module Raygeo (
    Transformation(..),
    stay, translate, stripTrans,
    Vector(..),
    normalize, vcosphi, origo,
    vlen, closestVector,
    trans, itrans, dotp, add, sub
) where

import Data.List

data Transformation = Transformation {
    a11  :: Float, a12  :: Float, a13  :: Float, a14  :: Float,
    a21  :: Float, a22  :: Float, a23  :: Float, a24  :: Float,
    a31  :: Float, a32  :: Float, a33  :: Float, a34  :: Float,
    a41  :: Float, a42  :: Float, a43  :: Float, a44  :: Float,

    ia11 :: Float, ia12 :: Float, ia13 :: Float, ia14 :: Float,
    ia21 :: Float, ia22 :: Float, ia23 :: Float, ia24 :: Float,
    ia31 :: Float, ia32 :: Float, ia33 :: Float, ia34 :: Float,
    ia41 :: Float, ia42 :: Float, ia43 :: Float, ia44 :: Float
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

translate :: Float -> Float -> Float -> Transformation
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
data Vector = Vector Float Float Float Float deriving (Show)

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

dotp :: Vector -> Vector -> Float
dotp (Vector x1 y1 z1 h1) (Vector x2 y2 z2 h2) = x1*x2+y1*y2+z1*z2

vcosphi :: Vector -> Vector -> Float
vcosphi v1 v2 = dotp v1 v2 / (vlen v1 * vlen v2)

add :: Vector -> Vector -> Vector
add (Vector x1 y1 z1 h1) (Vector x2 y2 z2 h2) = Vector (x1+x2) (y1+y2) (z1+z2) 1

sub :: Vector -> Vector -> Vector
sub (Vector x1 y1 z1 h1) (Vector x2 y2 z2 h2) = Vector (x1-x2) (y1-y2) (z1-z2) 1

normalize :: Vector -> Vector
normalize (Vector x y z h) = Vector (x/h) (y/h) (z/h) 1

-- Calculates the length of a vector
-- does not care about the h coordinate, it must be 1!
vlen :: Vector -> Float
vlen (Vector x y z h) = sqrt(x**2+y**2+z**2)

-- Returns the closest point to a given point from a point list
closestVector :: Vector -> [Vector] -> Vector
closestVector v vectors = foldl1' (\v1 v2 -> if vlen (sub v2 v) < vlen (sub v1 v) then v2 else v1) vectors
