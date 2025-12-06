module TerminalGraphics where
import Tri ( Tri, pointInsideTriDepth )
import Data.Maybe (isJust)

-- | Clears the terminal
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Gets a 3d perspective to display to the screen given 2d tris to display
getScreen :: [Tri] -> Int -> String
getScreen tris screenSize =
    unlines
        [ [ if any (isJust . pointInsideTriDepth
                     ( (fromIntegral x / (2 * fromIntegral screenSize)) - 0.5
                     , (fromIntegral y / fromIntegral screenSize) - 0.5
                     )
                  ) tris
                then '#'
                else ' '
          | x <- [0..2*screenSize]
          ]
        | y <- [0..screenSize]
        ]
