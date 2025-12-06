import Vector ( Vec3(..) )
import TerminalGraphics ( clearScreen, getScreen )
import Matrix ( symmetricPerspectiveMatrix )
import Tri
import Objects

-- String input to move the camera. Takes in string input and the current position to output the new position
move :: String -> Vec3 -> Vec3
move cmd (Vec3 x y z) =
    case cmd of
        "forward"  -> Vec3 x y (z + 1)
        "backward" -> Vec3 x y (z - 1)
        "left"     -> Vec3 (x + 1) y z
        "right"    -> Vec3 (x - 1) y z
        _          -> Vec3 x y z

-- | Recursive loop for each "frame" of the 3d game
loop :: Vec3 -> [Tri] -> IO ()
loop currentPos world = do
    clearScreen
    -- print current screen
    let screenMat = symmetricPerspectiveMatrix 2 0.1 2 10
    let relativeTris = map (`triSubVec` currentPos) world
    let screenTris = get2DTris screenMat relativeTris
    putStrLn (getScreen screenTris 10)
    -- print current pos
    putStrLn $ "Current position: " ++ show currentPos
    putStrLn "Enter command (forward/backward/quit): "
    cmd <- getLine
    if cmd == "quit"
        then putStrLn "Exiting."
        else loop (move cmd currentPos) world

-- | Create World
createWorld :: [Tri]
createWorld =
    let
        bigCube = mapTri (*2) cube
        bigFarCube = mapTri (+ Vec3 0 0 (-20)) bigCube
    in bigFarCube

-- | Entry point
main :: IO ()
main = do
    loop (Vec3 0 0 0) createWorld

