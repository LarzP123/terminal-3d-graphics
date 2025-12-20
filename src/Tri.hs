{-# LANGUAGE InstanceSigs #-}
module Tri where
import Vector
import Matrix

-- | A Triangle made of 3 vertices of type 'a' in space, with a color
data Tri a = Tri a a a Char deriving Show

instance Functor Tri where
    fmap :: (a -> b) -> Tri a -> Tri b
    fmap f (Tri a b c color) = Tri (f a) (f b) (f c) color

-- | barycentric-coordinate point-in-triangle test + depth interpolation
pointInsideTriDepth :: (Double, Double) -> Tri Vec3 -> Maybe Double
pointInsideTriDepth (px, py)
    (Tri (Vec3 x1 y1 z1)
         (Vec3 x2 y2 z2)
         (Vec3 x3 y3 z3) _) =

    let
        v0x = x2 - x1; v0y = y2 - y1
        v1x = x3 - x1; v1y = y3 - y1
        v2x = px - x1; v2y = py - y1

        dot00 = v0x*v0x + v0y*v0y
        dot01 = v0x*v1x + v0y*v1y
        dot02 = v0x*v2x + v0y*v2y
        dot11 = v1x*v1x + v1y*v1y
        dot12 = v1x*v2x + v1y*v2y

        denom = dot00 * dot11 - dot01 * dot01
    in
        if denom < 0.0001 then Nothing
        else
            let v   = (dot11 * dot02 - dot01 * dot12) / denom
                w   = (dot00 * dot12 - dot01 * dot02) / denom
                u   = 1 - v - w
                eps = 1e-9
                z   = u*z1 + v*z2 + w*z3
            in if u >= -eps && v >= -eps && w >= -eps && z > 0.1
                then Just z
                else Nothing

-- | Converts a series of 3d triangles to 2d triangles given a camera perspective
get2DTris :: Mat4 -> [Tri Vec3] -> [Tri Vec3]
get2DTris perspectiveMat = map (\(Tri a b c color) -> Tri (multMatVec3 perspectiveMat a 1)
            (multMatVec3 perspectiveMat b 1)
            (multMatVec3 perspectiveMat c 1) color)
