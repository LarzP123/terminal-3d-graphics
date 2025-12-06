module Tri where
import Vector ( Vec4(Vec4), Vec3(..), Dot(dot) )
import Matrix ( Mat4, multMatVec )

-- | A Triangle made of 3 vector points in space
data Tri = Tri Vec3 Vec3 Vec3 Char deriving Show

-- | Triangle minus a vector coordiante. Just shifts the triangle around
triSubVec :: Tri -> Vec3 -> Tri
triSubVec (Tri a b c color) v = Tri (a-v) (b-v) (c-v) color

-- | barycentric-coordinate point-in-triangle test + depth interpolation
pointInsideTriDepth :: (Double, Double) -> Tri -> Maybe Double
pointInsideTriDepth (px, py)
    (Tri p1@(Vec3 _ _ z1 )
         p2@(Vec3 _ _ z2)
         p3@(Vec3 _ _ z3) _) =
    let
        -- Convert point to Vec 3
        p  = Vec3 px py 0

        -- Edge vectors
        v0 = p3 - p1
        v1 = p2 - p1
        v2 = p  - p1

        -- Dot products
        dot00 = dot v0 v0
        dot01 = dot v0 v1
        dot02 = dot v0 v2
        dot11 = dot v1 v1
        dot12 = dot v1 v2

        denom = dot00 * dot11 - dot01 * dot01

        u = (dot11 * dot02 - dot01 * dot12) / denom
        v = (dot00 * dot12 - dot01 * dot02) / denom
        w = 1 - u - v
    in
        if u >= 0 && v >= 0 && w >= 0
            then Just (w*z1 + v*z2 + u*z3)
            else Nothing

-- | Convert 3D space point to screen coordinates
spacePointToScreen :: Mat4 -> Vec3 -> Vec3
spacePointToScreen perspectiveMat (Vec3 a b c) =
    let Vec4 rx ry rz rw = multMatVec perspectiveMat coord4D
    in Vec3 (rx / rw) (ry / rw) (rz / rw)
    where coord4D = Vec4 a b c 1

-- | Converts a series of 3d triangles to 2d triangles given a camera perspective
get2DTris :: Mat4 -> [Tri] -> [Tri]
get2DTris perspectiveMat = map (\(Tri a b c color) -> Tri (spacePointToScreen perspectiveMat a)
            (spacePointToScreen perspectiveMat b)
            (spacePointToScreen perspectiveMat c) color)

-- | Allows you to easily map a math function onto all the coordinates of a triangle
mapTri :: (Vec3 -> Vec3) -> [Tri] -> [Tri]
mapTri func tris = [ Tri (func a) (func b) (func c) color | Tri a b c color <- tris ]
