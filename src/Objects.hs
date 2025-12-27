module Objects where
import Tri
import Vector
import Textures

-- | Make a cube with 6 faces, each made of 2 triangles, using the same texture
cubeFormer :: [[RGB]] -> [Tri Vec3]
cubeFormer tex =
    let
        -- Default UVs for a quad split into two triangles
        uvA = (Vec2 0 0, Vec2 1 0, Vec2 1 1)
        uvB = (Vec2 0 0, Vec2 1 1, Vec2 0 1)

        mkFace :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Tri Vec3]
        mkFace v0 v1 v2 v3 =
            [ Tri v0 v1 v2 (Texture (TextureMapping tex (fst3 uvA) (snd3 uvA) (trd3 uvA)))
            , Tri v0 v2 v3 (Texture (TextureMapping tex (fst3 uvB) (snd3 uvB) (trd3 uvB)))
            ]

        -- helpers to unpack triples
        fst3 (x,_,_) = x
        snd3 (_,y,_) = y
        trd3 (_,_,z) = z

        -- Cube corners
        p000 = Vec3 (-10) (-10) (-10)
        p001 = Vec3 (-10) (-10) 10
        p010 = Vec3 (-10) 10 (-10)
        p011 = Vec3 (-10) 10 10
        p100 = Vec3 10 (-10) (-10)
        p101 = Vec3 10 (-10) 10
        p110 = Vec3 10 10 (-10)
        p111 = Vec3 10 10 10

    in concat
        [ mkFace p001 p101 p111 p011  -- Front
        , mkFace p100 p000 p010 p110  -- Back
        , mkFace p000 p001 p011 p010  -- Left
        , mkFace p101 p100 p110 p111  -- Right
        , mkFace p011 p111 p110 p010  -- Top
        , mkFace p000 p100 p101 p001  -- Bottom
        ]

{-
tree :: [Tri Vec3]
tree =
    [
        ------------------------------------------------------------------
        -- TRUNK  (rectangular prism from y = -10 to y = 0)
        ------------------------------------------------------------------
        -- Dimensions:
        --   x: [-2, 2]
        --   z: [-2, 2]
        --   y: [-10, 0]
        -- Color: 'b' (b)

        -- Front face (z = 2)
        Tri (Vec3 (-2) (-10) 2) (Vec3 2 (-10) 2) (Vec3 2 0 2) 'b',
        Tri (Vec3 (-2) (-10) 2) (Vec3 2 0 2) (Vec3 (-2) 0 2) 'b',

        -- Back face (z = -2)
        Tri (Vec3 2 (-10) (-2)) (Vec3 (-2) (-10) (-2)) (Vec3 (-2) 0 (-2)) 'b',
        Tri (Vec3 2 (-10) (-2)) (Vec3 (-2) 0 (-2)) (Vec3 2 0 (-2)) 'b',

        -- Left face (x = -2)
        Tri (Vec3 (-2) (-10) (-2)) (Vec3 (-2) (-10) 2) (Vec3 (-2) 0 2) 'b',
        Tri (Vec3 (-2) (-10) (-2)) (Vec3 (-2) 0 2) (Vec3 (-2) 0 (-2)) 'b',

        -- Right face (x = 2)
        Tri (Vec3 2 (-10) 2) (Vec3 2 (-10) (-2)) (Vec3 2 0 (-2)) 'b',
        Tri (Vec3 2 (-10) 2) (Vec3 2 0 (-2)) (Vec3 2 0 2) 'b',

        -- Top of trunk (y = 0)
        Tri (Vec3 (-2) 0 (-2)) (Vec3 2 0 (-2)) (Vec3 2 0 2) 'b',
        Tri (Vec3 (-2) 0 (-2)) (Vec3 2 0 2) (Vec3 (-2) 0 2) 'b',

        ------------------------------------------------------------------
        -- LEAVES  (pyramid from y=0 to y=12)
        ------------------------------------------------------------------
        -- Base: square [-8,8] x [-8,8] at y=0
        -- Apex: (0, 12, 0)
        -- Color: 'g' (green)

        -- Front triangle
        Tri (Vec3 (-8) 0  8) (Vec3 8 0  8) (Vec3 0 12 0) 'g',

        -- Right triangle
        Tri (Vec3 8 0  8) (Vec3 8 0 (-8)) (Vec3 0 12 0) 'g',

        -- Back triangle
        Tri (Vec3 8 0 (-8)) (Vec3 (-8) 0 (-8)) (Vec3 0 12 0) 'g',

        -- Left triangle
        Tri (Vec3 (-8) 0 (-8)) (Vec3 (-8) 0  8) (Vec3 0 12 0) 'g'
    ]
-}
