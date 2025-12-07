module Tri where
import Vector
import Matrix ( Mat4, multMatVec )

-- | A Triangle made of 3 vector points in space
data Tri = Tri Vec3 Vec3 Vec3 Char deriving Show

-- | Triangle minus a vector coordiante. Just shifts the triangle around
triSubVec :: Tri -> Vec3 -> Tri
triSubVec (Tri a b c color) v = Tri (a-v) (b-v) (c-v) color

-- | barycentric-coordinate point-in-triangle test + depth interpolation
pointInsideTriDepth :: (Double, Double) -> Tri -> Maybe Double
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
mapTris :: (Vec3 -> Vec3) -> [Tri] -> [Tri]
mapTris func tris = [ Tri (func a) (func b) (func c) color | Tri a b c color <- tris ]
