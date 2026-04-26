{-|
Module      : Terminal3D
Description : A simple terminal 3D rasteriser with textures, portals, and lighting.
Stability   : experimental

Re-exports every public sub-module so library users only need one import:

> import Terminal3D

== Quick-start

@
import Terminal3D

main :: IO ()
main = do
    tex <- readBMP "myTexture.bmp"
    let world  = cubeFormer tex
        lights = [Ray (Vec3 0.5 1 0.5), Ambient 0.3]
        litWorld = map (bakeLight lights) world
    evalStateT (loop litWorld) (Vec3 0 0 (-30), Vec3 0 0 0, Perspective, (80, 40))
@
-}
module Terminal3D
    ( -- * Re-exports
      module Terminal3D.Vector
    , module Terminal3D.Matrix
    , module Terminal3D.Textures
    , module Terminal3D.Tri
    , module Terminal3D.Lighting
    , module Terminal3D.TerminalGraphics
    , module Terminal3D.Objects
    ) where

import Terminal3D.Vector
import Terminal3D.Matrix
import Terminal3D.Textures
import Terminal3D.Tri
import Terminal3D.Lighting
import Terminal3D.TerminalGraphics
import Terminal3D.Objects
