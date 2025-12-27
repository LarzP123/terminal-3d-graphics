{-# LANGUAGE InstanceSigs #-}
module Tri where
import Vector
import Matrix
import Textures

-- | A Triangle made of 3 vertices of type 'a' in space, with a texture and a 2d mapping onto
data Tri a = Tri a a a (ColorMapping Vec2) deriving Show

instance Functor Tri where
    fmap :: (a -> b) -> Tri a -> Tri b
    fmap f (Tri a b c texMap) = Tri (f a) (f b) (f c) texMap

-- | Compute barycentric coordinates and depth for a point inside a triangle
barycentricDepth :: Vec2 -> Tri Vec3 -> Maybe Vec4
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

                zVec = component3 vLast vecA vecB vecC
                z = Vec3 u v w `dot` zVec
            in Just (Vec4 u v w z)

-- | Check if barycentric coordinates are inside the triangle
insideTriangle :: Vec3 -> Bool
insideTriangle (Vec3 u v w) =
    let eps = 1e-9
    in u >= -eps && v >= -eps && w >= -eps

-- | Sample a pixel from a [[RGB]] grid given a UV coordinate
sampleTexture :: [[RGB]] -> Vec2 -> RGB
sampleTexture pixels (Vec2 u v) =
    let h = length pixels
        w = if null pixels then 0 else length (head pixels)
        i = min (h-1) $ max 0 $ round (v * fromIntegral (h-1))
        j = min (w-1) $ max 0 $ round (u * fromIntegral (w-1))
    in (pixels !! i) !! j

-- | Interpolate UV coordinates using barycentric weights
interpolateUV :: Vec3 -> ColorMapping Vec2 -> Vec2
interpolateUV _ (Solid _) = Vec2 0 0  -- unused
interpolateUV bary (Texture (TextureMapping _ uvA uvB uvC)) =
    let
        xVec = component3 vFirst uvA uvB uvC
        yVec = component3 vLast uvA uvB uvC
    in Vec2 (bary `dot` xVec) (bary `dot` yVec)

-- | Return the RGB color at a point inside a triangle, along with interpolated depth
pointInsideTriColor :: Vec2 -> Tri Vec3 -> ColorMapping Vec2 -> Maybe (RGB, Double)
pointInsideTriColor p tri colorMapping = do
    barycentricCoords <- barycentricDepth p tri
    if not (insideTriangle (toVec3 barycentricCoords) && vLast barycentricCoords > 0.1) then Nothing
    else
        let rgb = case colorMapping of
                    Solid c -> c
                    Texture (TextureMapping texturePixels _ _ _) ->
                        sampleTexture texturePixels (interpolateUV (toVec3 barycentricCoords) colorMapping)
        in Just (rgb, vLast barycentricCoords)

-- | Converts a series of 3d triangles to 2d triangles given a camera perspective
get2DTris :: Mat4 -> [Tri Vec3] -> [Tri Vec3]
get2DTris perspectiveMat = map (\(Tri a b c color) -> Tri (multMatVec3 perspectiveMat a 1)
            (multMatVec3 perspectiveMat b 1)
            (multMatVec3 perspectiveMat c 1) color)
