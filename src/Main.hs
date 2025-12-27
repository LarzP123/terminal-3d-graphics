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

-- | The game loop using StateT
loop :: [Tri Vec3] -> StateT (Vec3, Vec3) IO ()
loop world = do
    -- get current state
    (currentPos, currentRot) <- get
    -- clear screen
    liftIO clearScreen
    -- compute screen
    let screenMat = symmetricPerspectiveMatrix 1 0.4 1 200
        movedTris = (fmap . fmap) (\v -> v - currentPos) world
        viewTris = (fmap . fmap) (\v -> multMatVec3 (viewMatrix currentRot) v 1) movedTris
        clippedViewTris = clipBehindCamera viewTris
        screenTris = get2DTris screenMat clippedViewTris
    -- print screen
    liftIO (putStrLn (getScreen screenTris (100, 100)))
    liftIO (putStrLn ("Current position: " ++ show currentPos))
    liftIO (putStrLn ("Current rotation: " ++ show currentRot))
    liftIO (putStrLn "Enter command (forward/backward/quit): ")
    -- get input
    cmd <- liftIO getLine
    if cmd == "quit"
        then liftIO (putStrLn "Exiting.")
        else do
            -- update state
            modify (\_ -> move cmd currentPos currentRot)
            loop world

-- | Create World with a cube and a room
createWorld :: IO [Tri Vec3]
createWorld = do
    -- Load textures
    cubeTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\cube.bmp"
    wallTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\wall.bmp"
    floorTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\floor.bmp"
    -- Cube in the center
    let cube = cubeFormer cubeTexture
    -- Room dimensions
        roomMin = Vec3 (-50) (-20) (-75)
        roomMax = Vec3 50 50 50
    -- Floor
        roomFloor = wallFormer floorTexture
                (comp3Reduce roomMin roomMin roomMin)
                (comp3Reduce roomMax roomMin roomMin)
                (comp3Reduce roomMax roomMin roomMax)
                (comp3Reduce  roomMin roomMin roomMax)
    -- Walls (front, back, left, right)
        wallFront = wallFormer wallTexture
                    (comp3Reduce roomMin roomMin roomMax)
                    (comp3Reduce roomMax roomMin roomMax)
                    (comp3Reduce roomMax roomMax roomMax)
                    (comp3Reduce roomMin roomMax roomMax)
        wallBack = wallFormer wallTexture
                    (comp3Reduce roomMax roomMin roomMin)
                    (comp3Reduce roomMin roomMin roomMin)
                    (comp3Reduce roomMin roomMax roomMin)
                    (comp3Reduce roomMax roomMax roomMin)
        wallLeft = wallFormer wallTexture
                    (comp3Reduce roomMin roomMin roomMin)
                    (comp3Reduce roomMin roomMin roomMax)
                    (comp3Reduce roomMin roomMax roomMax)
                    (comp3Reduce roomMin roomMax roomMin)
        wallRight = wallFormer wallTexture
                    (comp3Reduce roomMax roomMin roomMax)
                    (comp3Reduce roomMax roomMin roomMin)
                    (comp3Reduce roomMax roomMax roomMin)
                    (comp3Reduce roomMax roomMax roomMax)
    return (cube ++ roomFloor ++ wallFront ++ wallBack ++ wallLeft ++ wallRight)

-- | Entry point
main :: IO ()
main = do
    world <- createWorld
    evalStateT (loop world) (Vec3 0 20 (-30), Vec3 0.0 0 0)