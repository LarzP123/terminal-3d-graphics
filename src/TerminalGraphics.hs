module TerminalGraphics where
import Tri
import Vector (Vec3)

-- | Clears the terminal
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

getColorOfPixel :: Double -> Double -> [Tri Vec3] -> Char
getColorOfPixel x y tris =
    let px = x - 0.5
        py = y - 0.5

        -- depth + letter pairs
        candidates :: [(Double, Char)]
        candidates =
            [ (d, c)
            | tri@(Tri _ _ _ c) <- tris
            , Just d <- [pointInsideTriDepth (px, py) tri]
            ]
    in
        case candidates of
            []    -> ' '
            _     -> snd (maximum candidates)

colorToANSI16SGRCode :: Char -> Bool -> Int
colorToANSI16SGRCode color foreground =
    base + if foreground then 0 else 10
  where
    base =
        case color of
            'k' -> 30  -- black
            'r' -> 31  -- red
            'g' -> 32  -- green
            'y' -> 33  -- yellow
            'b' -> 34  -- blue
            'm' -> 35  -- magenta
            'c' -> 36  -- cyan
            'w' -> 97  -- white
            _   -> 97  -- default white

getColored2Pixel :: Int -> Int -> [Tri Vec3] -> Int -> String
getColored2Pixel xPix yPix tris screenSize =
    let
        xRel      = fromIntegral xPix / fromIntegral screenSize
        yTopRel   = fromIntegral yPix / fromIntegral screenSize
        yBotRel   = fromIntegral yPix / fromIntegral screenSize
        colorTop  = getColorOfPixel xRel yTopRel tris
        colorBot  = getColorOfPixel xRel yBotRel tris
        colorTopNum = colorToANSI16SGRCode colorTop True
        colorBotNum = colorToANSI16SGRCode colorBot False
    in "\ESC[" ++ show colorTopNum ++ ";" ++ show colorBotNum ++ "m▀\ESC[0m"


getScreen :: [Tri Vec3] -> Int -> String
getScreen tris screenSize =
    unlines
        [ concatMap (\x -> getColored2Pixel x y tris screenSize)
                    [0 .. screenSize]
        | y <- [0,2 .. screenSize]
        ]
