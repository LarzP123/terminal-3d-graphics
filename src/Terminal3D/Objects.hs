module Terminal3D.Objects where

import Terminal3D.Tri
import Terminal3D.Vector
import Terminal3D.Textures
import Terminal3D.Matrix

-- | Build a textured quad (two triangles) from four corner vertices and a texture
texWallFormer :: Texture -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Tri Vec3]
texWallFormer tex v0 v1 v2 v3 =
    [ Tri v0 v1 v2 (Texture (TextureMapping tex (Vec2 0 0) (Vec2 1 0) (Vec2 1 1)))
    , Tri v0 v2 v3 (Texture (TextureMapping tex (Vec2 0 0) (Vec2 1 1) (Vec2 0 1)))
    ]

-- | Build a textured quad (two triangles) from four corner vertices and a solid
solWallFormer :: RGB -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Tri Vec3]
solWallFormer rgb v0 v1 v2 v3 =
    [ Tri v0 v1 v2 (Solid rgb)
    , Tri v0 v2 v3 (Solid rgb)
    ]

type WallFormer = Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Tri Vec3]

treeFormer :: WallFormer -> WallFormer -> [Tri Vec3]
treeFormer trunkFormer leafFormer =
        trunkFormer -- Trunk
            (Vec3 (-trunkW) (-trunkH) 0)
            (Vec3   trunkW  (-trunkH) 0)
            (Vec3   trunkW    0       0)
            (Vec3 (-trunkW)   0       0)
        ++ trunkFormer
            (Vec3 0 (-trunkH) (-trunkW))
            (Vec3 0 (-trunkH)   trunkW)
            (Vec3 0   0         trunkW)
            (Vec3 0   0       (-trunkW))
        ++ leafFormer -- Left Leaf
            (Vec3 0 leafH 0) (Vec3 (-leafW) 0 0) (Vec3 leafW 0 0) (Vec3 0 leafH 0)
        ++ leafFormer -- Front Leaf
            (Vec3 0 leafH 0) (Vec3 0 0 (-leafW)) (Vec3 0 0 leafW) (Vec3 0 leafH 0)
    where
        trunkH = 4
        trunkW = 2
        leafH  = 14
        leafW  = 8

-- | Makes island
islandFormer :: Double -> Double -> WallFormer -> WallFormer -> [Tri Vec3]
islandFormer radius height sideFormer baseFormer =
    let angle i  = (pi / 3) * fromIntegral i
        basePt i = Vec3 (radius * cos (angle i)) 0 (radius * sin (angle i))
        apex     = Vec3 0 (-height) 0
        baseCtr  = Vec3 0 0 0
        sides = concat
            [ fmap flipTri (sideFormer (basePt i) (basePt (i + 1)) apex apex)
            | i <- [0 :: Int .. 5]
            ]
        base = concat
            [ fmap flipTri (baseFormer baseCtr (basePt i) (basePt (i + 1)) baseCtr)
            | i <- [0 :: Int .. 5]
            ]
    in sides ++ base

-- | Builds a room from a Vec3 featuring one corner and another with the opposite corner
roomFormer :: Vec3 -> Vec3 -> WallFormer -> WallFormer -> [Tri Vec3]
roomFormer roomMin roomMax floorFormer wallFormer =
    let 
        roomFloor = fmap flipTri (floorFormer
            (comp3Reduce roomMin roomMin roomMin)
            (comp3Reduce roomMax roomMin roomMin)
            (comp3Reduce roomMax roomMin roomMax)
            (comp3Reduce roomMin roomMin roomMax))
        wallFront = wallFormer
            (comp3Reduce roomMin roomMin roomMax)
            (comp3Reduce roomMax roomMin roomMax)
            (comp3Reduce roomMax roomMax roomMax)
            (comp3Reduce roomMin roomMax roomMax)
        wallBack  = wallFormer
            (comp3Reduce roomMax roomMin roomMin)
            (comp3Reduce roomMin roomMin roomMin)
            (comp3Reduce roomMin roomMax roomMin)
            (comp3Reduce roomMax roomMax roomMin)
        wallLeft  = wallFormer
            (comp3Reduce roomMin roomMin roomMin)
            (comp3Reduce roomMin roomMin roomMax)
            (comp3Reduce roomMin roomMax roomMax)
            (comp3Reduce roomMin roomMax roomMin)
        wallRight = wallFormer
            (comp3Reduce roomMax roomMin roomMax)
            (comp3Reduce roomMax roomMin roomMin)
            (comp3Reduce roomMax roomMax roomMin)
            (comp3Reduce roomMax roomMax roomMax)
    in concat [roomFloor, wallFront, wallBack, wallLeft, wallRight]


-- | Build a pair of linked portal quads with decorative borders
portalFormer :: WallFormer -> WallFormer -> (Vec3, Vec3) -> (Vec3, Vec3) -> Bool -> [Tri Vec3]
portalFormer texAFormer texBFormer (vA0, vA2) (vB0, vB2) flipPortal =
    let vA1 = Vec3 (vF vA0) (vM vA2) (vL vA0)
        vA3 = Vec3 (vF vA2) (vM vA0) (vL vA2)
        vB1 = Vec3 (vF vB0) (vM vB2) (vL vB0)
        vB3 = Vec3 (vF vB2) (vM vB0) (vL vB2)
        rotSrc          = triToBasisMat (vA0, vA1, vA2)
        rotDst          = triToBasisMat (if flipPortal then (vB0, vB2, vB1) else (vB0, vB1, vB2))
        portalRotMatrix = rotSrc <> transposeMat4 rotDst
    in [ Tri vA0 vA1 vA2 (Portal vB0 vB1 vB2 portalRotMatrix)
       , Tri vA0 vA2 vA3 (Portal vB0 vB2 vB3 portalRotMatrix)
       , Tri vB0 vB1 vB2 (Portal vA0 vA1 vA2 portalRotMatrix)
       , Tri vB0 vB2 vB3 (Portal vA0 vA2 vA3 portalRotMatrix)
       ]
       ++ borderFormer texAFormer (vA0, vA1, vA2, vA3) True
       ++ borderFormer texBFormer (vB0, vB1, vB2, vB3) False

-- | Build a slightly-scaled border quad around a portal face
borderFormer :: WallFormer -> (Vec3, Vec3, Vec3, Vec3) -> Bool -> [Tri Vec3]
borderFormer wallFormer (v0, v1, v2, v3) flipBool =
    let normNotDirec = (v3 - v0) `cross` (v2 - v0)
        direc        = if flipBool then negate normNotDirec else normNotDirec
        borderOffset = vMap (*0.01) (signum direc)
        center       = vMap (/2) (v0 + v2)
        scalOp       = (* 1.2)
        b0 = vMap scalOp (v0 - center) + center + borderOffset
        b1 = vMap scalOp (v1 - center) + center + borderOffset
        b2 = vMap scalOp (v2 - center) + center + borderOffset
        b3 = vMap scalOp (v3 - center) + center + borderOffset
    in wallFormer b0 b1 b2 b3

-- | Build a textured unit cube centred at the origin (side length 20)
cubeFormer :: WallFormer -> [Tri Vec3]
cubeFormer sideFormer =
    let p000 = Vec3 (-10) (-10) (-10); p001 = Vec3 (-10) (-10) 10
        p010 = Vec3 (-10)  10  (-10);  p011 = Vec3 (-10)  10   10
        p100 = Vec3  10  (-10) (-10);  p101 = Vec3  10  (-10)  10
        p110 = Vec3  10   10  (-10);   p111 = Vec3  10   10    10
    in concat
        [ sideFormer p001 p101 p111 p011           -- Front
        , fmap flipTri (sideFormer p100 p000 p010 p110) -- Back
        , sideFormer p000 p001 p011 p010           -- Left
        , sideFormer p101 p100 p110 p111           -- Right
        , sideFormer p011 p111 p110 p010           -- Top
        , sideFormer p000 p100 p101 p001           -- Bottom
        ]
