module TerminalGraphics where
import Tri

-- | Clears the terminal
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

getScreen :: [Tri] -> Int -> String
getScreen tris screenSize =
    unlines
        [ [ pixelChar x y
          | x <- [0 .. 2 * screenSize]
          ]
        | y <- [0 .. screenSize]
        ]
  where
    pixelChar :: Int -> Int -> Char
    pixelChar x y =
        let px = (fromIntegral x / (2 * fromIntegral screenSize)) - 0.5
            py = (fromIntegral y / fromIntegral screenSize) - 0.5

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

