module Color (
    Color(..),
    cadd, cfilter, cscale, isIntense
) where

-- Color in R G B format
-- values ranges from 0 to 1
data Color = Color !Double !Double !Double deriving (Show)

-- Adds two colors, like combining two lights
cadd :: Color -> Color -> Color
cadd (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)

-- Filters a color with an other. This is useful for
-- subtractive color mixing.
cfilter :: Color -> Color -> Color
cfilter (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)

-- Scale the color intensity
cscale :: Double -> Color -> Color
cscale s (Color r g b) = Color (s*r) (s*g) (s*b)

-- Tells if a color is more intense then a treshold
isIntense :: Double -> Color -> Bool
isIntense t (Color r g b) = r+g+b > t
