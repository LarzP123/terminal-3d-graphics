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

-- ---------------------------------------------------------------------------
-- New building blocks (used by the castle, fire, teapot and rainbow worlds)
-- ---------------------------------------------------------------------------

-- | Build a box between two opposite corners (min corner first) by stretching the cube
boxFormer :: Vec3 -> Vec3 -> WallFormer -> [Tri Vec3]
boxFormer (Vec3 x0 y0 z0) (Vec3 x1 y1 z1) sideFormer =
    (fmap . fmap) (\(Vec3 x y z) -> Vec3 (cx + x * sx) (cy + y * sy) (cz + z * sz)) (cubeFormer sideFormer)
    where
        cx = (x0 + x1) / 2
        cy = (y0 + y1) / 2
        cz = (z0 + z1) / 2
        sx = (x1 - x0) / 20
        sy = (y1 - y0) / 20
        sz = (z1 - z0) / 20

-- | Build a four sided pyramid from the centre of its base, half the base width and its height
pyramidFormer :: Vec3 -> Double -> Double -> WallFormer -> [Tri Vec3]
pyramidFormer (Vec3 cx cy cz) halfW height sideFormer =
    let base = [ Vec3 (cx + halfW) cy (cz + halfW), Vec3 (cx - halfW) cy (cz + halfW)
               , Vec3 (cx - halfW) cy (cz - halfW), Vec3 (cx + halfW) cy (cz - halfW) ]
        apex = Vec3 cx (cy + height) cz
    in concat [ sideFormer b' b apex apex | (b, b') <- zip base (drop 1 base ++ take 1 base) ]

-- | Join two matching rings of points with a band of quads
bandFormer :: [Vec3] -> [Vec3] -> WallFormer -> [Tri Vec3]
bandFormer ringA ringB sideFormer =
    let next ring = drop 1 ring ++ take 1 ring
    in concat [ sideFormer a' a b b'
              | ((a, a'), (b, b')) <- zip (zip ringA (next ringA)) (zip ringB (next ringB)) ]

-- | Spin a list of (radius, height) points around the y axis to make a pot-like shape
latheFormer :: [(Double, Double)] -> WallFormer -> [Tri Vec3]
latheFormer profile sideFormer =
    let ring (r, y) = [ Vec3 (r * cos a) y (r * sin a) | k <- [0 .. 7 :: Int], let a = (pi / 4) * fromIntegral k ]
        rings = map ring profile
    in concat (zipWith (\lo hi -> bandFormer lo hi sideFormer) rings (drop 1 rings))

-- | A castle: a keep, four corner towers with pointed roofs and walls between the towers
castleFormer :: WallFormer -> WallFormer -> [Tri Vec3]
castleFormer wallFormer roofFormer =
    let tower (x, z) = boxFormer (Vec3 (x - 6) 0 (z - 6)) (Vec3 (x + 6) 30 (z + 6)) wallFormer
            ++ pyramidFormer (Vec3 x 30 z) 7 14 roofFormer
        keep = boxFormer (Vec3 (-15) 0 (-15)) (Vec3 15 40 15) wallFormer
            ++ pyramidFormer (Vec3 0 40 0) 16 20 roofFormer
        curtain = concat
            [ boxFormer (Vec3 (-30) 0 (-31)) (Vec3 30 12 (-29)) wallFormer
            , boxFormer (Vec3 (-30) 0 29)    (Vec3 30 12 31)    wallFormer
            , boxFormer (Vec3 (-31) 0 (-30)) (Vec3 (-29) 12 30) wallFormer
            , boxFormer (Vec3 29 0 (-30))    (Vec3 31 12 30)    wallFormer
            ]
    in keep ++ curtain ++ concatMap tower [ (x, z) | x <- [-30, 30], z <- [-30, 30] ]

-- | A campfire: crossed logs with red, orange and yellow flames
fireFormer :: WallFormer -> WallFormer -> WallFormer -> WallFormer -> [Tri Vec3]
fireFormer logFormer redFormer orangeFormer yellowFormer =
    let logs = boxFormer (Vec3 (-12) 0 (-2)) (Vec3 12 4 2) logFormer
            ++ boxFormer (Vec3 (-2) 0 (-12)) (Vec3 2 4 12) logFormer
        flame (x, z, w, h, former) = pyramidFormer (Vec3 x 4 z) w h former
    in logs ++ concatMap flame
        [ (-6, 3, 5, 16, redFormer), (6, -3, 5, 18, redFormer), (3, 7, 4, 12, redFormer), (-4, -7, 4, 14, redFormer)
        , (0, 0, 5, 26, orangeFormer), (-2, 2, 3, 20, orangeFormer), (2, -2, 3, 22, orangeFormer)
        , (0, 0, 2.5, 34, yellowFormer)
        ]

-- | A teapot: round body with lid and knob, plus a spout and a handle in a second style
teapotFormer :: WallFormer -> WallFormer -> [Tri Vec3]
teapotFormer bodyFormer trimFormer = body ++ spout ++ handle
    where
        body = latheFormer
            [ (0, 0), (7, 0), (11, 4), (12, 9), (10, 14), (7, 16)   -- base and belly
            , (7, 17), (4, 19), (2, 19), (2, 21), (0, 22) ]         -- lid and knob
            bodyFormer
        spout = bandFormer
            [Vec3 10 3 (-2), Vec3 10 3 2, Vec3 10 8 2, Vec3 10 8 (-2)]
            [Vec3 19 13 (-1), Vec3 19 13 1, Vec3 19 16 1, Vec3 19 16 (-1)]
            trimFormer
        handle = boxFormer (Vec3 (-17) 11 (-1.5)) (Vec3 (-9) 13 1.5) trimFormer
            ++ boxFormer (Vec3 (-17) 4 (-1.5)) (Vec3 (-15) 13 1.5) trimFormer
            ++ boxFormer (Vec3 (-17) 3 (-1.5)) (Vec3 (-9) 5 1.5) trimFormer

-- | One ring of the rainbow: a thick arched band between an inner and outer radius
ringFormer :: (Double, Double) -> WallFormer -> [Tri Vec3]
ringFormer (rIn, rOut) former = concat [ bandFormer s0 s1 former | (s0, s1) <- zip slices (drop 1 slices) ]
    where
        slices = map slice [0 .. 12 :: Int]
        slice k = [point rIn (-2) k, point rOut (-2) k, point rOut 2 k, point rIn 2 k]
        point r z k = Vec3 (r * cos (angle k)) (r * sin (angle k)) z
        angle k = pi * fromIntegral k / 12

-- | A rainbow arch made of one ring per style given (outermost first)
rainbowFormer :: [WallFormer] -> [Tri Vec3]
rainbowFormer bandFormers = concat [ ringFormer (rOut - 3.6, rOut) former | (rOut, former) <- zip [59.6, 55.6 ..] bandFormers ]

-- | Repeat a wall former over a grid of tiles (cols along the first edge, rows along the last) so a texture repeats instead of stretching
tiledFormer :: Int -> Int -> WallFormer -> WallFormer
tiledFormer cols rows former v0 v1 _ v3 =
    concat [ former (at i j) (at (i + 1) j) (at (i + 1) (j + 1)) (at i (j + 1))
           | i <- [0 .. cols - 1], j <- [0 .. rows - 1] ]
    where
        at i j = v0 + vMap (* (fromIntegral i / fromIntegral cols)) (v1 - v0)
                    + vMap (* (fromIntegral j / fromIntegral rows)) (v3 - v0)

-- | A winding river of lava made of flat slabs lying on the ground (y = 0), running from the back of the room to the front
lavaFormer :: WallFormer -> [Tri Vec3]
lavaFormer flowFormer = concatMap slab
    [ (-48, -100, -32, -58), (-48, -58, 32, -42), (16, -42, 32, 10), (-60, 10, 32, 26), (-60, 26, -44, 100) ]
    where
        slab (x0, z0, x1, z1) = fmap flipTri (tiledFormer (tiles (x1 - x0)) (tiles (z1 - z0)) flowFormer
            (Vec3 x0 0 z0) (Vec3 x1 0 z0) (Vec3 x1 0 z1) (Vec3 x0 0 z1))
        tiles len = max 1 (round (len / 16))