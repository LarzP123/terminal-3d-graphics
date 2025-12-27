import Vector
import TerminalGraphics ( clearScreen, getScreen )
import Matrix
import Tri
import Objects
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Textures

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

-- | The game loop using StateT
loop :: [Tri Vec3] -> StateT (Vec3, Vec3) IO ()
loop world = do
    -- get current state
    (currentPos, currentRot) <- get
    -- clear screen
    liftIO clearScreen
    -- compute screen
    let screenMat = symmetricPerspectiveMatrix 1 0.6 1 5
    let movedTris = (fmap . fmap) (\v -> v - currentPos) world
    let viewTris = (fmap . fmap) (\v -> multMatVec3 (viewMatrix currentRot) v 1) movedTris
    let clippedViewTris = clipBehindCamera viewTris
    let screenTris = get2DTris screenMat clippedViewTris
    -- print screen
    liftIO $ putStrLn (getScreen screenTris (40, 40))
    liftIO $ putStrLn $ "Current position: " ++ show currentPos
    liftIO $ putStrLn $ "Current rotation: " ++ show currentRot
    liftIO $ putStrLn "Enter command (forward/backward/quit): "
    -- get input
    cmd <- liftIO getLine
    if cmd == "quit"
        then liftIO $ putStrLn "Exiting."
        else do
            -- update state
            modify (\_ -> move cmd currentPos currentRot)
            loop world

-- | Create World with textured cube
createWorld :: IO [Tri Vec3]
createWorld = do
    -- Load texture
    cubeTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\grass.bmp"
    -- Form cube with texture
    let cube = cubeFormer cubeTexture
    -- Optionally translate cube in Z
    return cube

-- | Entry point
main :: IO ()
main = do
    world <- createWorld
    evalStateT (loop world) (Vec3 (-35) (-35) 0, Vec3 0.8 (-1.0) 0)