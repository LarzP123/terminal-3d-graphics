# Terminal3DGraphics

This is a little 3D engine written in Haskell that draws straight into your terminal. You can walk around the scene with keyboard input, and it all renders in true-colour using ANSI half-block characters (▀), which lets each character cell show two stacked "pixels". This works on both Linux and Windows and also exports as a library if anyone wants to make custom 3D worlds.

![Demo Portal World with Recursive Portal](SampleImages/Portal.png)

![Demo Chess World in Low Resolution](SampleImages/Chess.png)

![Demo Rainbow World](SampleImages/Rainbow.png)

# Features

 - Movement around the worlds is done via entering keys on the keyboard and pressing enter. 
 - Allows for Portals with recursive views through
 - Allows for selections of post processing and super sampling anti-aliasing
 - Has multiple demo worlds
 - Translations are selectable in the program
 - Allows customizable screen size
 - Pipelines auto-build versions for X64 Linux and Windows

## Building the demo

To build with Cabal and have it actually run quickly

```bash
cabal build all
cabal run t3d
```

To run with the interpreter

```bash
ghci -isrc -iapp -package parallel -package bytestring -package transformers -package array -package deepseq -package comonad -package process app/Main.hs
:main
```

## Using as a library

Add to your `your-project.cabal`:

```cabal
build-depends: terminal-3d-graphics
```

Then in your Haskell source:

```haskell
import Terminal3D
import Control.Monad.Trans.State

main :: IO ()
main = do
    tex <- readBMP "my-texture.bmp"
    let world    = cubeFormer tex
        lights   = [Ray (Vec3 0.5 1 0.5), Ambient 0.3]
        litWorld = map (bakeLight lights) world
    evalStateT (myLoop litWorld) (Vec3 0 0 (-30), Vec3 0 0 0, Perspective, (80, 40))
```
