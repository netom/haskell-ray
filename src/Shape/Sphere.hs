module Shape.Sphere (
    Sphere(..)
) where

import Shape
import Vector

data Sphere = Sphere

instance Shape Sphere where
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
            solutions = Data.List.filter (> 0.00001) $ solveQuadratic
                (rdx**2+rdy**2+rdz**2)
                (2*rx*rdx+2*ry*rdy+2*rz*rdz)
                (rx**2+ry**2+rz**2-1)

