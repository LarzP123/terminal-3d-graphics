module TerminalGraphics where
import Tri
import Vector
import Textures
import Control.Parallel.Strategies

-- | Clears the terminal
clearScreen :: IO ()
clearScreen = putStr (concat (replicate 50 "\n") ++ "\ESC[2J\ESC[H")

-- | Get the RGB color of the nearest triangle at a pixel
getColorOfPixel :: Vec2 -> [Tri Vec4] -> Projection -> RGB
getColorOfPixel p tris proj =
    let candidates =
            [ (d, rgb)
            | Tri a b c colorMapping <- tris
            , Just (rgb, d) <- [pointInsideTriColor p (Tri a b c colorMapping) colorMapping proj]
            ]
    in case candidates of
        [] -> RGB { red = 0, green = 0, blue = 0 }  -- default black
        _  -> snd (maximum candidates)

-- | Convert an RGB to a true color ANSI SGR code
colorToANSITRUE :: RGB -> Bool -> String
colorToANSITRUE (RGB r g b) foreground =
    let mode = if foreground then (38 :: Integer) else 48  -- 38 = foreground, 48 = background
    in "\ESC[" ++ show mode ++ ";2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

-- | Convert pixel coordinates to normalized screen space centered at 0.5
toScreenRel :: (Int, Int) -> (Int, Int) -> Vec2
toScreenRel (x, y) (screenWidth, screenHeight) =
    Vec2 ((fromIntegral x / fromIntegral screenWidth) - 0.5)
         ((fromIntegral y / fromIntegral screenHeight) - 0.5)

-- | Get a colored pixel using true-color, centered properly
getColored2Pixel :: (Int, Int) -> [Tri Vec4] -> (Int, Int) -> Projection -> String
getColored2Pixel pixCoords tris screenDimensions proj =
    let
        Vec2 xRel yRel = toScreenRel pixCoords screenDimensions

        color = getColorOfPixel (Vec2 xRel yRel) tris proj

        fgCode = colorToANSITRUE color True
        bgCode = colorToANSITRUE color False
    in fgCode ++ bgCode ++ "▀\ESC[0m"

-- | Render all triangles to screen (parallel rows)
getScreen :: [Tri Vec4] -> (Int, Int) -> Projection -> String
getScreen tris screenDimensions@(screenWidth, screenHeight) proj =
    unlines rows
  where
    rows :: [String]
    rows =
        parMap rdeepseq renderRow
            [0,2 .. screenHeight - 1]

    renderRow :: Int -> String
    renderRow y =
        concatMap
            (\x -> getColored2Pixel (x, y) tris screenDimensions proj)
            [0 .. screenWidth - 1]
