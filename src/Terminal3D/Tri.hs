module Terminal3D.Tri where

import Terminal3D.Vector
import Terminal3D.Matrix
import Terminal3D.Textures
import Data.Word
import Data.Array

-- | Projection mode for texture mapping
data Projection = Affine | Perspective

-- | Build a change-of-basis matrix from three triangle vertices
triToBasisMat :: (Vec3, Vec3, Vec3) -> Mat4
triToBasisMat (a, b, c) =
    let right  = signum (b - a)
        normal = signum ((b - a) `cross` (c - a))
        up     = normal `cross` right
    in case map (`toVec4` 0) [right, up, normal] of
         [r, u, n] -> Mat4 r u n (Vec4 0 0 0 1)
         _          -> error "Impossible"

{-| A tiny positive value used to keep geometry strictly in front of the camera,
    avoiding division-by-zero in perspective projection. -}
epsilon :: Double
epsilon = 0.2

-- | How a triangle's surface is coloured
data ColorMapping
    = Solid RGB
    | Texture (TextureMapping Vec2)
    | Portal Vec3 Vec3 Vec3 Mat4
    deriving Show

{-| A triangle made of three vertices of type @a@ plus a colour mapping.
    Texture UV coordinates correspond to vertices in declaration order. -}
data Tri a = Tri a a a ColorMapping deriving Show

-- | Surface normal of a world-space triangle (not normalised)
getNorm :: Tri Vec3 -> Vec3
getNorm (Tri v1 v2 v3 _) = (v2 - v1) `cross` (v3 - v1)

-- | Reverse a triangle's winding order (flips its normal)
flipTri :: Tri a -> Tri a
flipTri (Tri vA vB vC (Texture (TextureMapping tex uvA uvB uvC))) =
    Tri vA vC vB (Texture (TextureMapping tex uvA uvC uvB))
flipTri (Tri vA vB vC (Solid col)) =
    Tri vA vC vB (Solid col)
flipTri (Tri vA vB vC (Portal v2A v2B v2C rotTrans)) =
    Tri vA vC vB (Portal v2A v2B v2C rotTrans)

instance Functor Tri where
    fmap f (Tri a b c texMap) = Tri (f a) (f b) (f c) texMap

-- | Extract the three vertices of a triangle
triToVec :: Tri a -> (a, a, a)
triToVec (Tri a b c _) = (a, b, c)

-- | Compute barycentric coordinates and depth for a point inside a projected triangle
barycentricDepth :: Vec2 -> Tri Vec4 -> Maybe Vec4
barycentricDepth p (Tri vecA vecB vecC _) =
    let vecAtoB = toVec2 (vecB - vecA)
        vecAtoC = toVec2 (vecC - vecA)
        vecAtoP = p - toVec2 vecA
        dot00 = vecAtoB `dot` vecAtoB
        dot01 = vecAtoB `dot` vecAtoC
        dot02 = vecAtoB `dot` vecAtoP
        dot11 = vecAtoC `dot` vecAtoC
        dot12 = vecAtoC `dot` vecAtoP
        denom = dot00 * dot11 - dot01 * dot01
    in if denom < 1e-8 then Nothing
       else
           let v = (dot11 * dot02 - dot01 * dot12) / denom
               w = (dot00 * dot12 - dot01 * dot02) / denom
               u = 1 - v - w
               zVec = component3 vZ (vecA, vecB, vecC)
               z    = Vec3 u v w `dot` zVec
           in Just (Vec4 u v w z)

-- | True when barycentric coordinates are inside (or on the edge of) the triangle
insideTriangle :: Vec3 -> Bool
insideTriangle (Vec3 u v w) =
    let eps = 1e-9 in u >= -eps && v >= -eps && w >= -eps

-- | Nearest-neighbour texture sample given a UV in [0,1]²
sampleTexture :: Texture -> Vec2 -> RGB
sampleTexture tex uvCoord =
    let ((_, _), (maxY, maxX)) = bounds tex
        pixelX = min maxX (max 0 (round (vF sloper)))
        pixelY = min maxY (max 0 (round (vL sloper)))
        sloper  = uvCoord * Vec2 (fromIntegral maxX) (fromIntegral maxY)
    in tex ! (pixelY, pixelX)

-- | Interpolate UV coordinates using barycentric weights (affine or perspective-correct)
interpolateUV :: Vec3 -> ColorMapping -> Vec3 -> Projection -> Vec2
interpolateUV _ (Solid _)    _ _           = Vec2 0 0
interpolateUV _ (Portal {})  _ _           = Vec2 0 0
interpolateUV bary (Texture (TextureMapping _ uvA uvB uvC)) _ Affine =
    let xVec = component3 vF (uvA, uvB, uvC)
        yVec = component3 vL (uvA, uvB, uvC)
    in Vec2 (bary `dot` xVec) (bary `dot` yVec)
interpolateUV bary (Texture (TextureMapping _ uvA uvB uvC)) w' Perspective =
    Vec2 (i vF) (i vL)
    where
        a'   = vMap (/ vF w') uvA
        b'   = vMap (/ vM w') uvB
        c'   = vMap (/ vL w') uvC
        invW = bary `dot` vMap recip w'
        i f  = bary `dot` component3 f (a', b', c') / invW

-- | Return the RGB colour and depth of the nearest hit on a triangle at pixel @p@
pointInsideTriColor
    :: Vec2 -> Tri Vec4 -> ColorMapping -> Projection
    -> [Tri Vec3] -> Word8 -> Mat4
    -> Maybe (RGB, Double)
pointInsideTriColor p tri colorMapping proj worldRegress regressCount rotRegress = do
    barycentricCoords <- barycentricDepth p tri
    if not (insideTriangle (toVec3 barycentricCoords) && vL barycentricCoords > epsilon)
        then Nothing
        else
            let rgb = case colorMapping of
                        Portal vAOut vBOut vCOut portalRotMatrix ->
                            let newRot      = portalRotMatrix <> rotRegress
                                mappedPoint = weight3 (vAOut, vBOut, vCOut) (toVec3 barycentricCoords)
                                ntcTris     = posRotToNtcTris worldRegress (mappedPoint, newRot)
                            in getColorOfPixel p ntcTris proj worldRegress (regressCount-1) newRot
                        Solid c -> c
                        Texture (TextureMapping texturePixels _ _ _) ->
                            let w'       = component3 vL (triToVec tri)
                                interpUv = interpolateUV (toVec3 barycentricCoords) colorMapping w' proj
                            in sampleTexture texturePixels interpUv
            in Just (rgb, vL barycentricCoords)

-- | Project a list of world-space triangles through a perspective matrix
get2DTris :: Vector spVec => Mat4 -> [Tri spVec] -> [Tri Vec4]
get2DTris perspectiveMat = map (\(Tri a b c color) ->
    Tri (multMatVec perspectiveMat (toVec4 a 1))
        (multMatVec perspectiveMat (toVec4 b 1))
        (multMatVec perspectiveMat (toVec4 c 1))
        color)

-- | Clip a triangle against the near plane (generic over space+UV vertex types)
clipTriGeneric
    :: (Vector spcVec, Vector mapVec)
    => ((spcVec, spcVec, spcVec), (mapVec, mapVec, mapVec))
    -> [((spcVec, spcVec, spcVec), (mapVec, mapVec, mapVec))]
clipTriGeneric ((vA, vB, vC), (v2A, v2B, v2C)) =
    let groupedVerts = [(vA,v2A), (vB,v2B), (vC,v2C)]
        frontVerts   = filter inFront groupedVerts
        backVerts    = filter (not . inFront) groupedVerts
    in case (frontVerts, backVerts) of
        ([(fA,fB)], [b1,b2]) ->
            let (i1A,i1B) = lerpVertGroupping (fA,fB) b1
                (i2A,i2B) = lerpVertGroupping (fA,fB) b2
            in [((fA,i1A,i2A),(fB,i1B,i2B))]
        ([(f1A,f1B),(f2A,f2B)], [b]) ->
            let (i1A,i1B) = lerpVertGroupping (f1A,f1B) b
                (i2A,i2B) = lerpVertGroupping (f2A,f2B) b
            in [((f1A,f2A,i2A),(f1B,f2B,i2B)),
                ((f1A,i2A,i1A),(f1B,i2B,i1B))]
        ([(f1A,f1B),(f2A,f2B),(f3A,f3B)], _) ->
            [((f1A,f2A,f3A),(f1B,f2B,f3B))]
        _ -> []
    where
        inFront (vec, _) = vZ vec > epsilon

-- | Clip a single triangle against the near plane
clipTri :: Tri Vec3 -> [Tri Vec3]
clipTri (Tri vA vB vC (Solid col)) =
    map (\((vA', vB', vC'), _) -> Tri vA' vB' vC' (Solid col))
        (clipTriGeneric ((vA,vB,vC),(Vec2 0 0,Vec2 0 0,Vec2 0 0)))
clipTri (Tri vA vB vC (Texture (TextureMapping tex v2A v2B v2C))) =
    map (\((vA', vB', vC'), (v2A', v2B', v2C')) ->
            Tri vA' vB' vC' (Texture (TextureMapping tex v2A' v2B' v2C')))
        (clipTriGeneric ((vA,vB,vC),(v2A,v2B,v2C)))
clipTri (Tri vA vB vC (Portal v2A v2B v2C rotMat)) =
    map (\((vA', vB', vC'), (v2A', v2B', v2C')) ->
            Tri vA' vB' vC' (Portal v2A' v2B' v2C' rotMat))
        (clipTriGeneric ((vA,vB,vC),(v2A,v2B,v2C)))

-- | Linearly interpolate between a front and back vertex at the near plane
lerpVertGroupping
    :: (Vector spVec, Vector mapVec)
    => (spVec, mapVec) -> (spVec, mapVec) -> (spVec, mapVec)
lerpVertGroupping (spVecFront, uvVecFront) (spVecBack, uvVecBack) =
    let t = if vZ (spVecFront - spVecBack) == 2*epsilon
                then 0.5
                else (vZ spVecFront - epsilon) / (vZ spVecFront - vZ spVecBack)
    in ( spVecFront + vMap (t*) (spVecBack - spVecFront)
       , uvVecFront + vMap (t*) (uvVecBack - uvVecFront) )

-- | Transform world triangles into normalised clip-space triangles for a given camera
posRotToNtcTris :: [Tri Vec3] -> (Vec3, Mat4) -> [Tri Vec4]
posRotToNtcTris world (pos, rotMat) =
    let screenMat       = symmetricPerspectiveMatrix 1 0.4 1 200
        movedTris       = (fmap . fmap) (\v -> v - pos) world
        viewTris        = (fmap . fmap) (rotateWorld rotMat) movedTris
        clippedViewTris = concatMap clipTri viewTris
        screenTris      = get2DTris screenMat clippedViewTris
        ntcTris         = (fmap . fmap) divW screenTris
    in ntcTris

-- | Find the RGB colour of the nearest triangle covering pixel @p@
getColorOfPixel :: Vec2 -> [Tri Vec4] -> Projection -> [Tri Vec3] -> Word8 -> Mat4 -> RGB
getColorOfPixel _ _ _ _ 0 _ = RGB { red = 160, green = 32, blue = 240 } -- portal depth limit: purple
getColorOfPixel p tris proj worldRegress regressCount rotRegress =
    let candidates =
            [ (d, rgb)
            | Tri a b c colorMapping <- tris
            , Just (rgb, d) <- [pointInsideTriColor p (Tri a b c colorMapping)
                                    colorMapping proj worldRegress regressCount rotRegress]
            ]
    in case candidates of
        [] -> RGB { red = 0, green = 0, blue = 0 }
        _  -> snd (maximum candidates)
