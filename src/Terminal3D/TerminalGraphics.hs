module Terminal3D.TerminalGraphics where

import Terminal3D.Tri
import Terminal3D.Vector
import Terminal3D.Textures
import Terminal3D.Matrix
import Control.Parallel.Strategies

-- | Clear the terminal using ANSI escape codes
clearScreen :: IO ()
clearScreen = putStr (concat (replicate 50 "\n") ++ "\ESC[2J\ESC[H")

-- | Convert an RGB value to a true-colour ANSI SGR escape sequence
colorToANSITRUE :: RGB -> Bool -> String
colorToANSITRUE (RGB r g b) foreground =
    let mode = if foreground then (38 :: Integer) else 48
    in "\ESC[" ++ show mode ++ ";2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

-- | Convert pixel coordinates to normalised screen space (centred at 0.5)
toScreenRel :: (Int, Int) -> (Int, Int) -> Vec2
toScreenRel (x, y) (screenWidth, screenHeight) =
    Vec2 ((fromIntegral x / fromIntegral screenWidth)  - 0.5)
         ((fromIntegral y / fromIntegral screenHeight) - 0.5)

-- | Render one pixel using true-colour ANSI and the half-block trick (▀)
getColored2Pixel
    :: (Int, Int) -> [Tri Vec4] -> (Int, Int)
    -> Projection -> [Tri Vec3] -> Mat4 -> String
getColored2Pixel pixCoords tris screenDimensions proj worldRegress rotRegress =
    let Vec2 xRel yRel = toScreenRel pixCoords screenDimensions
        color  = getColorOfPixel (Vec2 xRel yRel) tris proj worldRegress 6 rotRegress
        fgCode = colorToANSITRUE color True
        bgCode = colorToANSITRUE color False
    in fgCode ++ bgCode ++ "▀\ESC[0m"

-- | Render a full frame to a 'String', processing rows in parallel
getScreen
    :: [Tri Vec4] -> (Int, Int) -> Projection -> [Tri Vec3] -> Mat4 -> String
getScreen tris screenDimensions@(screenWidth, screenHeight) proj worldRegress rotRegress =
    unlines rows
  where
    rows :: [String]
    rows = parMap rdeepseq renderRow [0, 2 .. screenHeight - 1]

    renderRow :: Int -> String
    renderRow y =
        concatMap
            (\x -> getColored2Pixel (x, y) tris screenDimensions proj worldRegress rotRegress)
            [0 .. screenWidth - 1]
