Module Shape (
    Shape
) where

import Vector

-- When a ray hit something, it expressed
-- with an Incidence object
data Incidence = Incidence
    !Vector -- The point on the object's shape
    !Vector -- The normal at that point
    deriving (Show)

class Shape where
    incidence :: Ray -> Shape -> Maybe Incidence
