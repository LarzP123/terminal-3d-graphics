import Vector
import TerminalGraphics ( clearScreen, getScreen )
import Tri
import Objects
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Textures
import Lighting (bakeLight, Light (Ray, Ambient))
import Matrix (rotationMatrix)

-- String input to move the camera. Takes in string input and the current position to output the new position
move :: String -> Vec3 -> Vec3 -> (Vec3, Vec3)
move cmd pos@(Vec3 x y z) rot@(Vec3 pitch yaw roll) =
    case cmd of
        -- Strafing
        "forward"  -> (Vec3 x       y (z + speed), rot)
        "backward" -> (Vec3 x       y (z - speed), rot)
        "left"     -> (Vec3 (x + speed) y z      , rot)
        "right"    -> (Vec3 (x - speed) y z      , rot)
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
        speed = 5

-- | The game loop using StateT
loop :: [Tri Vec3] -> StateT (Vec3, Vec3, Projection, (Int, Int)) IO ()
loop world = do
    -- get current state
    (currentPos, currentRot, projection, screenSize) <- get
    -- clear screen
    liftIO clearScreen
    -- compute screen
    let rotMat = rotationMatrix currentRot
    let ntcTris = posRotToNtcTris world (currentPos, rotMat)
    -- print screen
    liftIO (putStrLn (getScreen ntcTris screenSize projection world rotMat))
    liftIO (putStrLn ("Current position: " ++ show currentPos))
    liftIO (putStrLn ("Current rotation: " ++ show currentRot))
    liftIO (putStrLn "Enter command (forward/backward/quit): ")
    -- get input
    cmd <- liftIO getLine
    if cmd == "quit"
        then liftIO (putStrLn "Exiting.")
        else do
            -- update state
            modify (\(pos, rot, proj, scrnSize) -> let (pos', rot') = move cmd pos rot in (pos', rot', proj, scrnSize))
            loop world

-- | Create World with a cube and a room
createWorld :: IO [Tri Vec3]
createWorld = do
    -- Load textures
    cubeTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\cube.bmp"
    wallTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\wall.bmp"
    floorTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\floor.bmp"
    portalOrangeTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\portalOrange.bmp"
    portalBlueTexture <- readBMP "C:\\Users\\a\\Documents\\C-Games\\haskell\\3DGraphicsTerminal\\Terminal3DGraphicsHaskell\\src\\textures\\portalBlue.bmp"
    -- Cube in the center
    let cube = cubeFormer cubeTexture
    -- Room dimensions
        roomMin = Vec3 (-50) (-20) (-75)
        roomMax = Vec3 50 50 50
    -- Floor
        roomFloor = fmap flipTri (wallFormer floorTexture
                (comp3Reduce roomMin roomMin roomMin)
                (comp3Reduce roomMax roomMin roomMin)
                (comp3Reduce roomMax roomMin roomMax)
                (comp3Reduce  roomMin roomMin roomMax))
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
        portalMin = Vec3 (-48) (-10) (-40)
        portalMax = Vec3 48 35 10
        portal = portalFormer portalBlueTexture portalOrangeTexture
                (comp3Reduce portalMin portalMin portalMax,
                comp3Reduce portalMin portalMax portalMin)
                (comp3Reduce portalMax portalMin portalMax,
                comp3Reduce portalMax portalMax portalMin)
        lights = [Ray (Vec3 0.25 0.25 0.25), Ambient 0.5]
        world = cube ++ roomFloor ++ wallFront ++ wallBack ++ wallLeft ++ wallRight ++ portal
        lightedWorld = fmap (bakeLight lights) world
    return lightedWorld

-- | Entry point
main :: IO ()
main = do
    world <- createWorld
    evalStateT (loop world) (Vec3 (-20) 15 (-15), Vec3 0.0 (-1.2) 0, Perspective, (1000, 1000))
