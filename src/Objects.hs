module Objects where
import Tri
import Vector
import Textures
import Matrix

-- | Make a textured quad (2 triangles) from 4 vertices and a texture
wallFormer :: [[RGB]] -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Tri Vec3]
wallFormer tex v0 v1 v2 v3 =
    [
        Tri v0 v1 v2 (Texture (TextureMapping tex (Vec2 0 0) (Vec2 1 0) (Vec2 1 1))),
        Tri v0 v2 v3 (Texture (TextureMapping tex (Vec2 0 0) (Vec2 1 1) (Vec2 0 1)))
    ]

-- | Make a quad (2 triangles) from 2 corners of each portal
portalFormer :: [[RGB]] -> [[RGB]] -> (Vec3, Vec3) -> (Vec3, Vec3) -> Bool -> [Tri Vec3]
portalFormer borderTexA borderTexB (vA0, vA2) (vB0, vB2) flipPortal =
    let -- Calculate the other 2 corners for each portal
        vA1 = Vec3 (vF vA0) (vM vA2) (vL vA0)
        vA3 = Vec3 (vF vA2) (vM vA0) (vL vA2)
        vB1 = Vec3 (vF vB0) (vM vB2) (vL vB0)
        vB3 = Vec3 (vF vB2) (vM vB0) (vL vB2)
        -- Get rotation matrix when going from portal A to portal B
        rotSrc = triToBasisMat (vA0, vA1, vA2)
        rotDst = triToBasisMat (if flipPortal then (vB0, vB2, vB1) else (vB0, vB1, vB2))
        portalRotMatrix = rotSrc <> transposeMat4 rotDst
        -- Get coords for the border quad
    in [
            Tri vA0 vA1 vA2 (Portal vB0 vB1 vB2 portalRotMatrix),
            Tri vA0 vA2 vA3 (Portal vB0 vB2 vB3 portalRotMatrix),
            Tri vB0 vB1 vB2 (Portal vA0 vA1 vA2 portalRotMatrix),
            Tri vB0 vB2 vB3 (Portal vA0 vA2 vA3 portalRotMatrix)
        ]
        ++ borderFormer borderTexA (vA0, vA1, vA2, vA3) True
        ++ borderFormer borderTexB (vB0, vB1, vB2, vB3) False

borderFormer :: [[RGB]] -> (Vec3, Vec3, Vec3, Vec3) -> Bool -> [Tri Vec3]
borderFormer tex (v0, v1, v2, v3) flipBool =
    let normNotDirec = (v3 - v0) `cross` (v2 - v0)
        direc = if flipBool then negate normNotDirec else normNotDirec
        borderOffset = vMap (*0.01) (signum direc)
        center = vMap (/2) (v0 + v2)
        scalOp = (* 1.2)
        b0 = vMap scalOp (v0 - center) + center + borderOffset
        b1 = vMap scalOp (v1 - center) + center + borderOffset
        b2 = vMap scalOp (v2 - center) + center + borderOffset
        b3 = vMap scalOp (v3 - center) + center + borderOffset
    in wallFormer tex b0 b1 b2 b3

cubeFormer :: [[RGB]] -> [Tri Vec3]
cubeFormer tex =
    let
        p000 = Vec3 (-10) (-10) (-10)
        p001 = Vec3 (-10) (-10) 10
        p010 = Vec3 (-10) 10 (-10)
        p011 = Vec3 (-10) 10 10
        p100 = Vec3 10 (-10) (-10)
        p101 = Vec3 10 (-10) 10
        p110 = Vec3 10 10 (-10)
        p111 = Vec3 10 10 10
    in concat
        [ wallFormer tex p001 p101 p111 p011  -- Front
        , fmap flipTri (wallFormer tex p100 p000 p010 p110)  -- Back
        , wallFormer tex p000 p001 p011 p010  -- Left
        , wallFormer tex p101 p100 p110 p111  -- Right
        , wallFormer tex p011 p111 p110 p010  -- Top
        , wallFormer tex p000 p100 p101 p001  -- Bottom
        ]

