module Objects where
import Tri
import Vector
import Textures

-- | Make a textured quad (2 triangles) from 4 vertices and a texture
wallFormer :: [[RGB]] -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Tri Vec3]
wallFormer tex v0 v1 v2 v3 =
    [
        Tri v0 v1 v2 (Texture (TextureMapping tex (Vec2 0 0) (Vec2 1 0) (Vec2 1 1))),
        Tri v0 v2 v3 (Texture (TextureMapping tex (Vec2 0 0) (Vec2 1 1) (Vec2 0 1)))
    ]

-- | Make a textured quad (2 triangles) from 4 vertices and a texture
portalFormer :: (Vec3, Vec3, Vec3, Vec3) -> (Vec3, Vec3, Vec3, Vec3) -> [Tri Vec3]
portalFormer (vA0, vA1, vA2, vA3) (vB0, vB1, vB2, vB3) =
    [
        Tri vA0 vA1 vA2 (Portal False vB0 vB0 vB2),
        Tri vA0 vA2 vA3 (Portal False vB0 vB2 vB3),
        Tri vB0 vB1 vB2 (Portal False vA0 vA0 vA2),
        Tri vB0 vB2 vB3 (Portal False vA0 vA2 vA3)
    ]

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

