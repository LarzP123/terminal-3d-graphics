module Lighting where
import Tri
import Textures
import Vector

data Light vec = Ray vec | Ambient Double

bakeLight :: [Light Vec3] -> Tri Vec3 -> Tri Vec3
bakeLight lights tri@(Tri v1 v2 v3 colorMap) =
    case colorMap of
        Solid rgb ->
            Tri v1 v2 v3 (Solid (brightMap (*mult) rgb))
        Texture (TextureMapping tex uvA uvB uvC) ->
            let
                bakedTex :: [[RGB]]
                bakedTex =
                    map (map (brightMap (*mult))) tex
            in
                Tri v1 v2 v3
                    (Texture (TextureMapping bakedTex uvA uvB uvC))
        Portal portFlip v1b v2b v3b rotMat -> -- Too complicated to have light go thorugh portal to light other side up
            Tri v1 v2 v3 (Portal portFlip v1b v2b v3b rotMat)
    where
        norm = getNorm tri
        mult = getNetBright lights norm

-- | Gets the net brightness on a surface given all of the lights on it and a vector normal to the surface
getNetBright :: Vector vec => [Light vec] -> vec -> Double
getNetBright lights surfNorm =
    let n = signum surfNorm
        contrib (Ray dir) = max 0 (dot n (signum dir))
        contrib (Ambient amb) = amb
    in sum (map contrib lights)
