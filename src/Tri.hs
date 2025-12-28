{-# LANGUAGE InstanceSigs #-}
module Tri where
import Vector
import Matrix
import Textures

{-| This is a number that is barely above zero. This is so we can do 3d graphics by making sure every object is
    This is so we can do 3d graphics by making sure every object is in front of you without dividing by zero. -}
epsilon :: Double
epsilon = 0.0001

{-| A Triangle made of 3 space vertices of type 'a' in space, with a color mapping onto it.
    The color mapping may either just be a solid color or a texture with 3 vec2s. Each space vertice maps to a 
    vec2 UV texture mapping based on the order they are listed in. -}
data Tri a = Tri a a a (ColorMapping Vec2) deriving Show

instance Functor Tri where
    fmap :: (a -> b) -> Tri a -> Tri b
    fmap f (Tri a b c texMap) = Tri (f a) (f b) (f c) texMap

{-| This extracts a list of space vertices and UV mapping vertices from a tri. This is so you can do
    transformations on the space vertices and UV vertices at the same time by having them paired.
    If you have a solid color mapping it will just return all of the UV vertices as 0 and create
    a texture with 1 pixel of that color. -}
triToVertGrouppings :: Tri a -> ([(a, Vec2)], [[RGB]])
triToVertGrouppings (Tri a b c (Solid color)) = ([(a, 0), (b, 0), (c, 0)], [[color]])
triToVertGrouppings (Tri v3a v3b v3c (Texture (TextureMapping tex v2a v2b v2c))) = ([(v3a, v2a), (v3b, v2b), (v3c, v2c)], tex)

-- | Turns space vertex and UV vertex mapping pairs and a texture int a Tri object
vertGrouppingsToTri :: (a, Vec2) -> (a, Vec2) -> (a, Vec2) -> [[RGB]] -> Tri a
-- If we have a bunch of zero vectors for the UV vectors, there is no real interpolation to be done and it will just be a solid color
vertGrouppingsToTri (v3a, 0) (v3b, 0) (v3c, 0) tex = Tri v3a v3b v3c (Solid ((head . head) tex))
-- If the texture is just one color, then we can just represent it as a type color and not need to do interpolation
vertGrouppingsToTri (v3a, _) (v3b, _) (v3c, _) tex | length tex == 1 && length (head tex) == 1 =
    Tri v3a v3b v3c (Solid (head (head tex)))
-- default case. This returns a texture mapping with uv coords
vertGrouppingsToTri (v3a, v2a) (v3b, v2b) (v3c, v2c) tex = Tri v3a v3b v3c (Texture (TextureMapping tex v2a v2b v2c))

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

                zVec = component3 vL vecA vecB vecC
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
        pixelY = min (imgHeight - 1) (max 0 (round (vL  sloper)))
    in  (pixels !! pixelY) !! pixelX

-- | Interpolate UV coordinates using barycentric weights
interpolateUV :: Vec3 -> ColorMapping Vec2 -> Vec2
interpolateUV _ (Solid _) = Vec2 0 0  -- unused
interpolateUV bary (Texture (TextureMapping _ uvA uvB uvC)) =
    let
        xVec = component3 vF uvA uvB uvC
        yVec = component3 vL uvA uvB uvC
    in Vec2 (bary `dot` xVec) (bary `dot` yVec)

-- | Return the RGB color at a point inside a triangle, along with interpolated depth
pointInsideTriColor :: Vec2 -> Tri Vec3 -> ColorMapping Vec2 -> Maybe (RGB, Double)
pointInsideTriColor p tri colorMapping = do
    barycentricCoords <- barycentricDepth p tri
    if not (insideTriangle (toVec3 barycentricCoords) && vL barycentricCoords > epsilon) then Nothing
    else
        let rgb = case colorMapping of
                    Solid c -> c
                    Texture (TextureMapping texturePixels _ _ _) ->
                        sampleTexture texturePixels (interpolateUV (toVec3 barycentricCoords) colorMapping)
        in Just (rgb, vL barycentricCoords)

-- | Converts a series of 3d triangles to 2d triangles given a camera perspective
get2DTris :: Mat4 -> [Tri Vec3] -> [Tri Vec4]
get2DTris perspectiveMat = map (\(Tri a b c color) -> Tri (multMatVec perspectiveMat (toVec4 a))
            (multMatVec perspectiveMat (toVec4 b))
            (multMatVec perspectiveMat (toVec4 c)) color)

-- | Clip triangles partially behind the camera and re-maps their textures
clipBehindCamera :: [Tri Vec3] -> [Tri Vec3]
clipBehindCamera = concatMap clipTri
    where
        clipTri :: Tri Vec3 -> [Tri Vec3]
        clipTri vec =
            let (verts, tex) = triToVertGrouppings vec
                frontVerts = filter inFront verts
                backVerts = filter (not . inFront) verts
            in case (frontVerts, backVerts) of
                ([f], [b1,b2]) ->
                    let i1 = lerpVertGroupping f b1
                        i2 = lerpVertGroupping f b2
                    in [vertGrouppingsToTri f i1 i2 tex]
                ([f1,f2], [b]) ->
                    let i1 = lerpVertGroupping f1 b
                        i2 = lerpVertGroupping f2 b
                    in [vertGrouppingsToTri f1 f2 i2 tex, vertGrouppingsToTri f1 i2 i1 tex]
                ([a,b,c], _) -> [vertGrouppingsToTri a b c tex]  -- fully in front
                _ -> [] -- Fully Behind
        -- testing if the 3d space point is in front of you. 2d space points don't have depth so discarded
        inFront (vec3, _) = vL vec3 > epsilon

-- | Interpolate between two vertices at z=0
lerpVertGroupping :: (Vec3, Vec2) -> (Vec3, Vec2) -> (Vec3, Vec2)
lerpVertGroupping (vec3Front, vec2Front) (vec3Back, vec2Back) =
    let t = if vL vec3Front == vL vec3Back
            then 0.5 -- midpoint as fallback
            else vL vec3Front / (vL vec3Front - vL vec3Back)
    in (Vec3
            (vF vec3Front + t * (vF vec3Back - vF vec3Front))
            (vM vec3Front + t * (vM vec3Back - vM vec3Front))
            epsilon,
        Vec2
            (vF vec2Front + t * (vF vec2Back - vF vec2Front))
            (vL vec2Front + t * (vL vec2Back - vL vec2Front))
    )
