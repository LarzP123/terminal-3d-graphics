import Vector ( Vec3(..) )
import TerminalGraphics ( clearScreen, getScreen )
import Matrix
import Tri
import Objects

-- String input to move the camera. Takes in string input and the current position to output the new position
move :: String -> Vec3 -> Vec3 -> (Vec3, Vec3)
move cmd pos@(Vec3 x y z) rot@(Vec3 pitch yaw roll) =
    case cmd of
        -- Strafing
        "forward"  -> (Vec3 x       y (z + 1), rot)
        "backward" -> (Vec3 x       y (z - 1), rot)
        "left"     -> (Vec3 (x + 1) y z      , rot)
        "right"    -> (Vec3 (x - 1) y z      , rot)
        -- Rotating
        "turn left"  -> (pos, Vec3 pitch (yaw - yawInc) roll)
        "turn right" -> (pos, Vec3 pitch (yaw + yawInc) roll)
        "turn up"    -> (pos, Vec3 (pitch + pitchInc) yaw roll)
        "turn down"  -> (pos, Vec3 (pitch - pitchInc) yaw roll)
        "lean left"  -> (pos, Vec3 pitch yaw (roll - rollInc))
        "lean right" -> (pos, Vec3 pitch yaw (roll + rollInc))
        -- Default
        _ -> (pos, rot)
    where
        pitchInc = 0.2
        yawInc = 0.2
        rollInc = 0.2


-- | Keep only triangles where all vertices have positive Z
clipBehindCamera :: [Tri Vec3] -> [Tri Vec3]
clipBehindCamera = filter allVerticesPositiveZ
  where
    allVerticesPositiveZ :: Tri Vec3 -> Bool
    allVerticesPositiveZ (Tri (Vec3 _ _ z1)
                               (Vec3 _ _ z2)
                               (Vec3 _ _ z3) _) =
      z1 > 0 && z2 > 0 && z3 > 0

-- | Recursive loop for each "frame" of the 3d game
loop :: (Vec3, Vec3) -> [Tri Vec3] -> IO ()
loop (currentPos, currentRot) world = do
    clearScreen
    -- print current screen
    let screenMat = symmetricPerspectiveMatrix 1 0.6 1 5
    let movedTris = (fmap . fmap) (\vec3 -> vec3 - currentPos) world
    let viewTris = (fmap . fmap) (\vec3 -> multMatVec3 (viewMatrix currentRot) vec3 1) movedTris
    let clippedViewTris = clipBehindCamera viewTris
    let screenTris = get2DTris screenMat clippedViewTris
    putStrLn (getScreen screenTris 30)
    -- print current pos
    putStrLn $ "Current position: " ++ show currentPos
    putStrLn $ "Current rotation: " ++ show currentRot
    putStrLn "Enter command (forward/backward/quit): "
    cmd <- getLine
    if cmd == "quit"
        then putStrLn "Exiting."
        else loop (move cmd currentPos currentRot) world

-- | Create World
createWorld :: [Tri Vec3]
createWorld =
    let
        farCube = (fmap . fmap) (+ Vec3 0 0 40) cube
    in farCube

-- | Entry point
main :: IO ()
main = do
    loop (Vec3 0 0 0, Vec3 0 0 0) createWorld

