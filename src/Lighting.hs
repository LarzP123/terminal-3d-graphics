module Lighting where
import Tri
import Textures
import Vector

data Light = Pos Vec3 Double | Ray Vec3 | Ambient Double

bakeLight :: [Light] -> Tri vec-> Tri vec
bakeLight _ (Tri v1 v2 v3 colorMap) =
    case colorMap of
        Solid rgb ->
            -- halve solid color too (consistent behavior)
            Tri v1 v2 v3 (Solid (brightMap (*0.5) rgb))

        Texture (TextureMapping tex uvA uvB uvC) ->
            let
                bakedTex :: [[RGB]]
                bakedTex =
                    map (map (brightMap (*0.5))) tex
            in
                Tri v1 v2 v3
                    (Texture (TextureMapping bakedTex uvA uvB uvC))
