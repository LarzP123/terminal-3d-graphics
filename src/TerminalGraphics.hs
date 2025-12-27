module TerminalGraphics where
import Tri
import Vector
import Textures

-- | Clears the terminal
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Get the RGB color of the nearest triangle at a pixel
getColorOfPixel :: Vec2 -> [Tri Vec3] -> RGB
getColorOfPixel p tris =
    let candidates =
            [ (d, rgb)
            | Tri a b c colorMapping <- tris
            , Just (rgb, d) <- [pointInsideTriColor p (Tri a b c colorMapping) colorMapping]
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
toScreenRel :: Int -> Int -> Int -> Int -> Vec2
toScreenRel x y screenWidth screenHeight =
    Vec2 ((fromIntegral x / fromIntegral screenWidth) - 0.5)
         ((fromIntegral y / fromIntegral screenHeight) - 0.5)

-- | Get a colored pixel using true-color, centered properly
getColored2Pixel :: Int -> Int -> [Tri Vec3] -> Int -> Int -> String
getColored2Pixel xPix yPix tris screenWidth screenHeight =
    let
        Vec2 xRel yRel = toScreenRel xPix yPix screenWidth screenHeight

        color = getColorOfPixel (Vec2 xRel yRel) tris

        fgCode = colorToANSITRUE color True
        bgCode = colorToANSITRUE color False
    in fgCode ++ bgCode ++ "▀\ESC[0m"

-- | Render all triangles to screen
getScreen :: [Tri Vec3] -> Int -> Int -> String
getScreen tris screenWidth screenHeight =
    unlines
        [ concatMap (\x -> getColored2Pixel x y tris screenWidth screenHeight)
                    [0 .. screenWidth - 1]
        | y <- [0,2 .. screenHeight - 1]
        ]
