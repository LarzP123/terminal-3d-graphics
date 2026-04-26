module Terminal3D.Lighting where

import Terminal3D.Tri
import Terminal3D.Textures
import Terminal3D.Vector

-- | A light source: either a directional ray or a uniform ambient term
data Light vec = Ray vec | Ambient Double

-- | Pre-bake lighting onto a triangle by scaling all its colour values
bakeLight :: [Light Vec3] -> Tri Vec3 -> Tri Vec3
bakeLight lights tri@(Tri v1 v2 v3 colorMap) =
    case colorMap of
        Solid rgb ->
            Tri v1 v2 v3 (Solid (brightMap (*mult) rgb))
        Texture (TextureMapping tex uvA uvB uvC) ->
            let bakedTex = map (map (brightMap (*mult))) tex
            in Tri v1 v2 v3 (Texture (TextureMapping bakedTex uvA uvB uvC))
        Portal v1b v2b v3b rotMat ->
            Tri v1 v2 v3 (Portal v1b v2b v3b rotMat)
    where
        norm = getNorm tri
        mult = getNetBright lights norm

-- | Compute net surface brightness given a list of lights and a surface normal
getNetBright :: Vector vec => [Light vec] -> vec -> Double
getNetBright lights surfNorm =
    let n = signum surfNorm
        contrib (Ray dir)    = max 0 (dot n (signum dir))
        contrib (Ambient amb) = amb
    in sum (map contrib lights)
