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
pointInsideTriDepth :: Vec2 -> Tri Vec3 -> Maybe Double
pointInsideTriDepth p (Tri vecA vecB vecC _) =
    let
        vecAtoB = vec3ToVec2 (vecB - vecA)
        vecAtoC = vec3ToVec2 (vecC - vecA)
        vecAtoP = p - vec3ToVec2 vecA

        dot00 = vecAtoB `dot` vecAtoB
        dot01 = vecAtoB `dot` vecAtoC
        dot02 = vecAtoB `dot` vecAtoP
        dot11 = vecAtoC `dot` vecAtoC
        dot12 = vecAtoC `dot` vecAtoP

        denom = dot00 * dot11 - dot01 * dot01
    in
        if denom < 1e-4 then Nothing
        else
            let v   = (dot11 * dot02 - dot01 * dot12) / denom
                w   = (dot00 * dot12 - dot01 * dot02) / denom
                u   = 1 - v - w
                eps = 1e-9
                z = u*zA + v*zB + w*zC
            in if u >= -eps && v >= -eps && w >= -eps && z > 0.1
                then Just z
                else Nothing
        where
            Vec3 _ _ zA = vecA
            Vec3 _ _ zB = vecB
            Vec3 _ _ zC = vecC

-- | Converts a series of 3d triangles to 2d triangles given a camera perspective
get2DTris :: Mat4 -> [Tri Vec3] -> [Tri Vec3]
get2DTris perspectiveMat = map (\(Tri a b c color) -> Tri (multMatVec3 perspectiveMat a 1)
            (multMatVec3 perspectiveMat b 1)
            (multMatVec3 perspectiveMat c 1) color)
