module Terminal3D.TerminalGraphics where

import Terminal3D.Tri
import Terminal3D.Vector
import Terminal3D.Textures
import Terminal3D.Matrix
import Control.Parallel.Strategies
import Control.Comonad
import qualified Data.ByteString.Builder as ByteBuilder
import qualified Data.ByteString.Lazy as LazyByteBuilder
import qualified Data.ByteString as StrictBS

-- | A 2D grid of values backed by a nested list
newtype Grid a = Grid { gData :: [[a]] }

instance Functor Grid where
    fmap f (Grid rows) = Grid ((fmap . fmap) f rows)

-- | 2D list index with edge clamping
gridAt :: Grid a -> Int -> Int -> a
gridAt (Grid rows) x y =
    let yOut = max 0 (min (length rows - 1) y)
        xOut = max 0 (min (length (head rows) - 1) x)
    in  rows !! yOut !! xOut

-- | A Grid with a focused point for comonadic extension
data FGrid a = FGrid (Int, Int) (Grid a)

instance Functor FGrid where
    fmap f (FGrid focus (Grid rows)) = FGrid focus (Grid ((fmap . fmap) f rows))

instance Comonad FGrid where
    extract (FGrid (c, r) g) = gridAt g c r
    duplicate fg@(FGrid _ (Grid rows)) =
        let h = length rows
            w = maybe 0 length (lookup (0 :: Int) (zip [0..] rows))
        in  FGrid (0, 0) (Grid [ [ FGrid (c, r) (unfocusGrid fg)
                                 | c <- [0 .. w - 1] ]
                               | r <- [0 .. h - 1] ])
    extend f = fmap f . duplicate

-- | Removes a focus point from a focused grid and just returns the grid
unfocusGrid :: FGrid a -> Grid a
unfocusGrid (FGrid _ g) = g

-- | Wraps a grid with a default focus at the corner
focusGrid :: Grid a -> FGrid a
focusGrid = FGrid (0, 0)

-- | Gets a weighted average of a list of RGB colours.
blendRGB :: [(Double, RGB)] -> RGB
blendRGB [] = RGB 0 0 0
blendRGB ws = RGB (chanC red) (chanC green) (chanC blue)
  where
    total   = sum (map fst ws)
    chanC f = round . max 0 $ sum [ w * fromIntegral (f c) | (w, c) <- ws ] / total

-- | Blends nearby grid cells using a weighted offset kernel
neighbourhood :: [(Int, Int, Double)] -> FGrid RGB -> RGB
neighbourhood offsets (FGrid (cx, cy) g) =
    blendRGB [ (w, gridAt g (cx+dc) (cy+dr)) | (dc, dr, w) <- offsets ]

data AntiAliasing = AntiAliasing {
        aaName :: String,
        aaSize :: Int,
        runAA  :: [(Int, Int, Double)]
    }

instance Show AntiAliasing where
    show = liftA2 (++) aaName $ (": " ++) . show . aaSize

-- | Creates a uniform box filter anti-aliasing with an nxn kernel
aaBox :: Int -> AntiAliasing
aaBox n = AntiAliasing "aaBox" n offsets
  where
    offsets
        | n <= 1    = [(0, 0, 1)]
        | even n    = runAA (aaBox (n - 1))
        | otherwise = [ (dc, dr, 1) | dr <- [-r..r], dc <- [-r..r] ]
    r = n `div` 2

-- | Creates a gaussian weighted anti-aliasing with an nxn kernel
aaGaussian :: Int -> AntiAliasing
aaGaussian n = AntiAliasing "aaGaussian" n offsets
    where
        offsets
            | n <= 1    = [(0, 0, 1)]
            | even n    = runAA (aaGaussian (n - 1))
            | otherwise = [ (dc, dr, w dc dr) | dr <- [-r..r], dc <- [-r..r] ]
        r       = n `div` 2
        sigma   = fromIntegral n / 6
        w dc dr = exp (negate (fromIntegral (dc*dc + dr*dr)) / (2 * sigma * sigma))

-- | Converts integer pixel offsets to normalised sub-pixel sample positions
toSubPixel :: [(Int, Int, Double)] -> [(Double, Double, Double)]
toSubPixel offsets = [ (fromIntegral dc / fromIntegral r, fromIntegral dr / fromIntegral r, w) | (dc, dr, w) <- offsets ]
    where r = maximum [ max (abs dc) (abs dr) | (dc, dr, _) <- offsets ] + 1

-- | Clear the terminal using ANSI escape codes
clearScreen :: IO ()
clearScreen = putStr (concat (replicate 50 "\n") ++ "\ESC[2J\ESC[H")

-- | Convert an RGB value to a true-colour ANSI SGR escape sequence
colorToANSITRUE :: RGB -> Bool -> String
colorToANSITRUE (RGB r g b) foreground =
    let mode = if foreground then (38 :: Integer) else 48
    in "\ESC[" ++ show mode ++ ";2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

-- | Convert pixel coordinates to normalised screen space (centred at 0.5)
toScreenRel :: (Double, Double) -> (Int, Int) -> Vec2
toScreenRel (x, y) (screenWidth, screenHeight) =
    Vec2 ((x / fromIntegral screenWidth)  - 0.5)
         ((y / fromIntegral screenHeight) - 0.5)

-- Converts RGB containning a property with a 256 possible value for each color to an ANSI string for the color
rgbToANSI :: RGB -> String
rgbToANSI color = colorToANSITRUE color True ++ colorToANSITRUE color False ++ "▀\ESC[0m"

-- Convert pixel to builder instead of String
rgbToBuilder :: RGB -> ByteBuilder.Builder
rgbToBuilder (RGB r g b) = mconcat [
        ByteBuilder.string7 "\ESC[38;2;",
        ByteBuilder.word8Dec r,
        ByteBuilder.char7 ';',
        ByteBuilder.word8Dec g,
        ByteBuilder.char7 ';',
        ByteBuilder.word8Dec b,
        ByteBuilder.string7 "m\ESC[48;2;",
        ByteBuilder.word8Dec r,
        ByteBuilder.char7 ';',
        ByteBuilder.word8Dec g,
        ByteBuilder.char7 ';',
        ByteBuilder.word8Dec b,
        ByteBuilder.string7 "m",
        ByteBuilder.byteString (StrictBS.pack [0xe2, 0x96, 0x80]),
        ByteBuilder.string7 "\ESC[0m"
    ]

-- | Render a full frame to a 'String', processing rows in parallel
getScreen :: [Tri Vec4] -> (Int, Int) -> Projection -> [Tri Vec3] -> Mat4 -> AntiAliasing -> AntiAliasing -> LazyByteBuilder.ByteString
getScreen tris screenDimensions@(screenWidth, screenHeight) proj worldRegress rotRegress ssaa ppaa =
    let samples = toSubPixel (runAA ssaa)
        rawGrid :: Grid RGB
        rawGrid = Grid (map renderRow [0, 2 .. screenHeight - 1] `using` parListChunk 8 rdeepseq)

        renderRow :: Int -> [RGB]
        renderRow y =
            [ blendRGB [ (w, let Vec2 xRel yRel = toScreenRel (fromIntegral x + dx, fromIntegral y + dy) screenDimensions
                in  getColorOfPixel (Vec2 xRel yRel) tris proj worldRegress 6 rotRegress)
                | (dx, dy, w) <- samples ]
            | x <- [0 .. screenWidth - 1] ]
    in ByteBuilder.toLazyByteString . foldMap (\row -> foldMap rgbToBuilder row <> ByteBuilder.char7 '\n')
        . gData . unfocusGrid $ extend (neighbourhood (runAA ppaa)) (focusGrid rawGrid)