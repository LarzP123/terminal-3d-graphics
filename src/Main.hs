import Vector ( Vec3(..) )
import TerminalGraphics ( clearScreen, getScreen )
import Matrix
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

-- | Keep only triangles where all vertices have positive Z
clipBehindCamera :: [Tri] -> [Tri]
clipBehindCamera = filter allVerticesPositiveZ
  where
    allVerticesPositiveZ :: Tri -> Bool
    allVerticesPositiveZ (Tri (Vec3 _ _ z1)
                               (Vec3 _ _ z2)
                               (Vec3 _ _ z3) _) =
      z1 > 0 && z2 > 0 && z3 > 0

-- | Recursive loop for each "frame" of the 3d game
loop :: Vec3 -> [Tri] -> IO ()
loop currentPos world = do
    clearScreen
    -- print current screen
    let screenMat = symmetricPerspectiveMatrix 1 0.4 1 10
    let movedTris = map (`triSubVec` currentPos) world
    let viewTris = mapTris (\v3 -> multMatVec3 (viewMatrix 0.0) v3 1) movedTris
    let clippedViewTris = clipBehindCamera viewTris
    let screenTris = get2DTris screenMat clippedViewTris
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
createWorld = cube
    -- let
        -- bigCube = mapTris (*2) cube
        -- bigFarCube = mapTris (+ Vec3 0 0 40) bigCube
    -- in cube

-- | Entry point
main :: IO ()
main = do
    loop (Vec3 (-20) 0 (-25)) createWorld

