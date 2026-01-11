import Vector
import TerminalGraphics ( clearScreen, getScreen )
import Matrix
import Tri
import Objects
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Textures
import Lighting (bakeLight, Light (Ray, Ambient))

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

posRotToNtcTris :: [Tri Vec3] -> (Vec3, Vec3) -> [Tri Vec4]
posRotToNtcTris world (pos, rot) =
    let screenMat = symmetricPerspectiveMatrix 1 0.4 1 200
        movedTris = (fmap . fmap) (\v -> v - pos) world
        viewTris = (fmap . fmap) (rotateWorld (rotationMatrix rot)) movedTris
        clippedViewTris = concatMap clipTri viewTris
        screenTris = get2DTris screenMat clippedViewTris
        ntcTris = (fmap .fmap) divW screenTris
    in ntcTris

-- | The game loop using StateT
loop :: [Tri Vec3] -> StateT (Vec3, Vec3, Projection, (Int, Int)) IO ()
loop world = do
    -- get current state
    (currentPos, currentRot, projection, screenSize) <- get
    -- clear screen
    liftIO clearScreen
    -- compute screen
    let ntcTris = posRotToNtcTris world (currentPos, currentRot)
    -- print screen
    liftIO (putStrLn (getScreen ntcTris screenSize projection world))
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
        portalMin = Vec3 (-48) (-10) (-10)
        portalMax = Vec3 48 15 10
        portalMinE = Vec3 (-49) (-12) (-12)
        portalMaxE = Vec3 49 17 12
        portalBorderOrange = wallFormer portalOrangeTexture
            (comp3Reduce portalMinE portalMinE portalMinE)
            (comp3Reduce portalMinE portalMinE portalMaxE)
            (comp3Reduce portalMinE portalMaxE portalMaxE)
            (comp3Reduce portalMinE portalMaxE portalMinE)
        portalBorderBlue = wallFormer portalBlueTexture
            (comp3Reduce portalMaxE portalMinE portalMinE)
            (comp3Reduce portalMaxE portalMinE portalMaxE)
            (comp3Reduce portalMaxE portalMaxE portalMaxE)
            (comp3Reduce portalMaxE portalMaxE portalMinE)
        portal = portalFormer
                    (comp3Reduce portalMin portalMin portalMin,
                    comp3Reduce portalMin portalMin portalMax,
                    comp3Reduce portalMin portalMax portalMax,
                    comp3Reduce portalMin portalMax portalMin)
                    (comp3Reduce portalMax portalMin portalMin,
                    comp3Reduce portalMax portalMin portalMax,
                    comp3Reduce portalMax portalMax portalMax,
                    comp3Reduce portalMax portalMax portalMin)
        lights = [Ray (Vec3 0.25 0.25 0.25), Ambient 0.5]
        world = cube ++ roomFloor ++ wallFront ++ wallBack ++ wallLeft ++ wallRight ++ portal ++ portalBorderOrange ++ portalBorderBlue
        lightedWorld = fmap (bakeLight lights) world
    return lightedWorld

-- | Entry point
main :: IO ()
main = do
    world <- createWorld
    evalStateT (loop world) (Vec3 0 20 (-30), Vec3 0.0 (-0.2) 0, Perspective, (100, 100))
