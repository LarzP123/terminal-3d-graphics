{-# LANGUAGE InstanceSigs #-}
module Tri where
import Vector
import Matrix
import Textures

-- https://en.wikipedia.org/wiki/Texture_mapping#Rasterisation_algorithms
data Projection = Affine | Perspective

{-| This is a number that is barely above zero. This is so we can do 3d graphics by making sure every object is
    This is so we can do 3d graphics by making sure every object is in front of you without dividing by zero. -}
epsilon :: Double
epsilon = 0.0001

data ColorMapping = Solid RGB | Texture (TextureMapping Vec2) | Portal Bool Vec3 Vec3 Vec3 deriving Show

{-| A Triangle made of 3 space vertices of type 'a' in space, with a color mapping onto it.
    The color mapping may either just be a solid color or a texture with 3 vec2s. Each space vertice maps to a 
    vec2 UV texture mapping based on the order they are listed in. -}
data Tri a = Tri a a a ColorMapping deriving Show

getNorm :: Tri Vec3 -> Vec3
getNorm (Tri v1 v2 v3 _) =
    let edge1 = v2 - v1 -- vector from v1 to v2
        edge2 = v3 - v1 -- vector from v1 to v3
    in cross edge1 edge2

-- | Flips the normal of a triangle by reversing its winding. Need this for lighting to be effecient
flipTri :: Tri a -> Tri a
flipTri (Tri vA vB vC (Texture(TextureMapping tex uvA uvB uvC))) = Tri vA vC vB (Texture(TextureMapping tex uvA uvC uvB))
flipTri (Tri vA vB vC (Solid col)) = Tri vA vC vB (Solid col)
flipTri (Tri vA vB vC (Portal flippedOut v2A v2B v2C)) = Tri vA vC vB (Portal (not flippedOut) v2A v2B v2C)

instance Functor Tri where
    fmap :: (a -> b) -> Tri a -> Tri b
    fmap f (Tri a b c texMap) = Tri (f a) (f b) (f c) texMap

triToVec :: Tri a -> (a, a, a)
triToVec (Tri a b c _) = (a, b, c)

-- | Compute barycentric coordinates and depth for a point inside a triangle
barycentricDepth :: Vec2 -> Tri Vec4 -> Maybe Vec4
barycentricDepth p (Tri vecA vecB vecC _) =
    let
        vecAtoB = toVec2 (vecB - vecA)
        vecAtoC = toVec2 (vecC - vecA)
        vecAtoP = p - toVec2 vecA

        dot00 = vecAtoB `dot` vecAtoB
        dot01 = vecAtoB `dot` vecAtoC
        dot02 = vecAtoB `dot` vecAtoP
        dot11 = vecAtoC `dot` vecAtoC
        dot12 = vecAtoC `dot` vecAtoP

        denom = dot00 * dot11 - dot01 * dot01
    in
        if denom < 1e-8 then Nothing
        else
            let
                v = (dot11 * dot02 - dot01 * dot12) / denom
                w = (dot00 * dot12 - dot01 * dot02) / denom
                u = 1 - v - w

                zVec = Vec3 (vZ vecA) (vZ vecB) (vZ vecC)
                z = Vec3 u v w `dot` zVec
            in Just (Vec4 u v w z)

-- | Check if barycentric coordinates are inside the triangle
insideTriangle :: Vec3 -> Bool
insideTriangle (Vec3 u v w) =
    let eps = 1e-9
    in u >= -eps && v >= -eps && w >= -eps

-- | Sample a pixel from a [[RGB]] grid given a UV coordinate
sampleTexture :: [[RGB]] -> Vec2 -> RGB
sampleTexture pixels uvCoord =
    let imgHeight = length pixels
        imgWidth  = length (head pixels)
        sloper = uvCoord * Vec2 (fromIntegral (imgWidth  - 1)) (fromIntegral (imgHeight - 1))
        pixelX = min (imgWidth  - 1) (max 0 (round (vF sloper)))
        pixelY = min (imgHeight - 1) (max 0 (round (vL sloper)))
    in  (pixels !! pixelY) !! pixelX

-- | Interpolate UV coordinates using barycentric weights (perspective-correct)
interpolateUV :: Vec3 -> ColorMapping -> Vec3 -> Projection -> Vec2
interpolateUV _ (Solid _) _ _ = Vec2 0 0 -- unused
interpolateUV _ (Portal { }) _ _ = Vec2 0 0 -- unused
interpolateUV bary (Texture (TextureMapping _ uvA uvB uvC)) _ Affine =
    let xVec = component3 vF (uvA, uvB, uvC)
        yVec = component3 vL (uvA, uvB, uvC)
    in Vec2 (bary `dot` xVec) (bary `dot` yVec)
interpolateUV bary (Texture (TextureMapping _ uvA uvB uvC)) w' Perspective = Vec2 (i vF) (i vL)
    where
        a' = vMap (/ vF w') uvA
        b' = vMap (/ vM w') uvB
        c' = vMap (/ vL w') uvC
        invW = bary `dot` vMap recip w'
        i f = bary `dot` component3 f (a', b', c') / invW

-- | Return the RGB color at a point inside a triangle, along with interpolated depth
pointInsideTriColor :: Vec2 -> Tri Vec4 -> ColorMapping -> Projection -> Maybe (RGB, Double)
pointInsideTriColor p tri colorMapping proj = do
    barycentricCoords <- barycentricDepth p tri
    if not (insideTriangle (toVec3 barycentricCoords) && vL barycentricCoords > epsilon) then Nothing
    else
        let rgb = case colorMapping of
                    Portal { } -> RGB { red = 50, green = 50, blue = 50 } -- THIS IS NOT DONE. SHOULD BE WAY MORE COMPLICATED
                    Solid c -> c
                    Texture (TextureMapping texturePixels _ _ _) ->
                        let w' = component3 vL (triToVec tri)
                            interpUv = interpolateUV (toVec3 barycentricCoords) colorMapping w' proj
                        in sampleTexture texturePixels interpUv
        in Just (rgb, vL barycentricCoords)

-- | Converts triangles with coordinates in n dimensional space to triangles projected onto 2d space given a camera perspective
get2DTris :: Vector spVec => Mat4 -> [Tri spVec] -> [Tri Vec4]
get2DTris perspectiveMat = map (\(Tri a b c color) -> Tri (multMatVec perspectiveMat (toVec4 a))
            (multMatVec perspectiveMat (toVec4 b))
            (multMatVec perspectiveMat (toVec4 c)) color)

clipTriGeneric :: (Vector mapVec) => ((Vec3, Vec3, Vec3), (mapVec, mapVec, mapVec)) -> [((Vec3, Vec3, Vec3), (mapVec, mapVec, mapVec))]
clipTriGeneric ((vA, vB, vC), (v2A, v2B, v2C)) =
    let
        groupedVerts = [(vA,v2A), (vB,v2B), (vC,v2C)]
        frontVerts = filter inFront groupedVerts
        backVerts = filter (not . inFront) groupedVerts
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
        ([(f1A,f1B),(f2A,f2B),(f3A,f3B)], _) -> [((f1A,f2A,f3A),(f1B,f2B,f3B))] -- fully in front
        _ -> [] -- Fully Behind
    where
        inFront (vec, _) = vZ vec > epsilon

clipTri :: Tri Vec3 -> [Tri Vec3]
clipTri (Tri vA vB vC (Solid col)) =
    map (\((vA', vB', vC'), (_, _, _)) -> Tri vA' vB' vC' (Solid col)) clippedCoords
    where clippedCoords = clipTriGeneric ((vA,vB,vC),(Vec2 0 0,Vec2 0 0,Vec2 0 0))
clipTri (Tri vA vB vC (Texture (TextureMapping tex v2A v2B v2C))) =
    map (\((vA', vB', vC'), (v2A', v2B', v2C')) -> Tri vA' vB' vC' (Texture (TextureMapping tex v2A' v2B' v2C'))) clippedCoords
    where clippedCoords = clipTriGeneric ((vA,vB,vC),(v2A,v2B,v2C))
clipTri (Tri vA vB vC (Portal flipPort v2A v2B v2C)) =
    map (\((vA', vB', vC'), (v2A', v2B', v2C')) -> Tri vA' vB' vC' (Portal flipPort v2A' v2B' v2C')) clippedCoords
    where clippedCoords = clipTriGeneric ((vA,vB,vC),(v2A,v2B,v2C))

-- | Clip triangles partially behind the camera and re-maps their textures
clipBehindCamera :: [Tri Vec3] -> [Tri Vec3]
clipBehindCamera = concatMap clipTri

-- | Interpolate between two vertices at z=0
lerpVertGroupping :: (Vector spVec, Vector mapVec) => (spVec, mapVec) -> (spVec, mapVec) -> (spVec, mapVec)
lerpVertGroupping (spVecFront, uvVecFront) (spVecBack, uvVecBack) =
    let t = if vZ (spVecFront - spVecBack) == 2*epsilon
            then 0.5 -- midpoint as fallback
            else (vZ spVecFront - 2*epsilon) / (vZ spVecFront - vZ spVecBack)
    in (spVecFront + vMap (t*) (spVecBack - spVecFront),
        uvVecFront + vMap (t*) (uvVecBack - uvVecFront))
