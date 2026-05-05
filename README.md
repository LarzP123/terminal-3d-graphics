# Terminal3DGraphics

A terminal 3D rasteriser written in Haskell.  
Features: perspective-correct texture mapping, near-plane clipping, baked directional + ambient lighting, and recursive portal rendering — all drawn in the terminal using true-colour ANSI half-blocks (▀).


## Running the demo

To build with Cabal and have it actually run quickly

```bash
# Build everything (library + executable)
cabal build all

# Run the demo directly (textures/ must be in your working directory)
cabal run t3d
```

To run with the interpreter

```bash
ghci -isrc -iapp -package parallel -package bytestring -package transformers -package array -package deepseq -package comonad -package process app/Main.hs
:main
```

---

## Using as a library

Add to your `your-project.cabal`:

```cabal
build-depends: terminal-3d-graphics
```

Then in your Haskell source:

```haskell
import Terminal3D   -- re-exports everything

import Control.Monad.Trans.State

main :: IO ()
main = do
    tex <- readBMP "my-texture.bmp"
    let world    = cubeFormer tex
        lights   = [Ray (Vec3 0.5 1 0.5), Ambient 0.3]
        litWorld = map (bakeLight lights) world
    evalStateT (myLoop litWorld) (Vec3 0 0 (-30), Vec3 0 0 0, Perspective, (80, 40))
```

Or import individual sub-modules for finer control:

```haskell
import Terminal3D.Vector
import Terminal3D.Tri
import Terminal3D.TerminalGraphics
```
