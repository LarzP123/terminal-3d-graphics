module Main where

import Terminal3D
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)

-- ---------------------------------------------------------------------------
-- Camera movement
-- ---------------------------------------------------------------------------

-- | Parse a movement command and return updated (position, rotation).
move :: String -> Vec3 -> Vec3 -> (Vec3, Vec3)
move cmd pos@(Vec3 x y z) rot@(Vec3 pitch yaw roll) =
    case cmd of
        "forward"    -> (Vec3 x       y       (z + speed), rot)
        "backward"   -> (Vec3 x       y       (z - speed), rot)
        "left"       -> (Vec3 (x + speed) y   z,           rot)
        "right"      -> (Vec3 (x - speed) y   z,           rot)
        "turn left"  -> (pos, Vec3 pitch (yaw - yawInc)   roll)
        "turn right" -> (pos, Vec3 pitch (yaw + yawInc)   roll)
        "turn up"    -> (pos, Vec3 (pitch + pitchInc) yaw roll)
        "turn down"  -> (pos, Vec3 (pitch - pitchInc) yaw roll)
        "lean left"  -> (pos, Vec3 pitch yaw (roll - rollInc))
        "lean right" -> (pos, Vec3 pitch yaw (roll + rollInc))
        _            -> (pos, rot)
    where
        speed    = 5
        pitchInc = 0.2
        yawInc   = 0.2
        rollInc  = 0.2

-- ---------------------------------------------------------------------------
-- Game loop
-- ---------------------------------------------------------------------------

-- | State: (cameraPosition, cameraRotation, projection, screenSize)
type AppState = (Vec3, Vec3, Projection, (Int, Int))

-- | Main render/input loop.
loop :: [Tri Vec3] -> StateT AppState IO ()
loop world = do
    (currentPos, currentRot, projection, screenSize) <- get
    liftIO clearScreen
    let rotMat  = rotationMatrix currentRot
        ntcTris = posRotToNtcTris world (currentPos, rotMat)
    liftIO $ putStrLn (getScreen ntcTris screenSize projection world rotMat)
    liftIO $ putStrLn ("Position : " ++ show currentPos)
    liftIO $ putStrLn ("Rotation : " ++ show currentRot)
    liftIO $ putStr   "Command (forward/backward/left/right/turn left/turn right/turn up/turn down/lean left/lean right/quit): "
    cmd <- liftIO getLine
    case cmd of
        "quit" -> liftIO (putStrLn "Goodbye." >> exitSuccess)
        _      -> do
            modify (\(p, r, pr, ss) ->
                let (p', r') = move cmd p r
                in (p', r', pr, ss))
            loop world

-- ---------------------------------------------------------------------------
-- World definition
-- ---------------------------------------------------------------------------

-- | Build the demo scene: a lit room containing a cube and two portals.
--   Texture paths are resolved relative to the working directory at runtime.
createWorld :: IO [Tri Vec3]
createWorld = do
    cubeTexture         <- readBMP "textures/cube.bmp"
    wallTexture         <- readBMP "textures/wall.bmp"
    floorTexture        <- readBMP "textures/floor.bmp"
    portalOrangeTexture <- readBMP "textures/portalOrange.bmp"
    portalBlueTexture   <- readBMP "textures/portalBlue.bmp"

    let cube    = cubeFormer cubeTexture
        roomMin = Vec3 (-50) (-20) (-75)
        roomMax = Vec3   50    50    50

        roomFloor = fmap flipTri (wallFormer floorTexture
                        (comp3Reduce roomMin roomMin roomMin)
                        (comp3Reduce roomMax roomMin roomMin)
                        (comp3Reduce roomMax roomMin roomMax)
                        (comp3Reduce roomMin roomMin roomMax))

        wallFront = wallFormer wallTexture
                        (comp3Reduce roomMin roomMin roomMax)
                        (comp3Reduce roomMax roomMin roomMax)
                        (comp3Reduce roomMax roomMax roomMax)
                        (comp3Reduce roomMin roomMax roomMax)
        wallBack  = wallFormer wallTexture
                        (comp3Reduce roomMax roomMin roomMin)
                        (comp3Reduce roomMin roomMin roomMin)
                        (comp3Reduce roomMin roomMax roomMin)
                        (comp3Reduce roomMax roomMax roomMin)
        wallLeft  = wallFormer wallTexture
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
        portalMax = Vec3   48    35    10
        portal    = portalFormer portalBlueTexture portalOrangeTexture
                        ( comp3Reduce portalMin portalMin portalMax
                        , comp3Reduce portalMin portalMax portalMin )
                        ( comp3Reduce portalMax portalMin portalMax
                        , comp3Reduce portalMax portalMax portalMin )
                        False

        lights    = [Ray (Vec3 0.25 0.25 0.25), Ambient 0.5]
        world     = cube ++ roomFloor ++ wallFront ++ wallBack
                       ++ wallLeft  ++ wallRight  ++ portal

    return (fmap (bakeLight lights) world)

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    world <- createWorld
    evalStateT (loop world)
        ( Vec3 (-20) 15 (-15)   -- initial position
        , Vec3 0.0 (-1.2) 0     -- initial rotation (pitch, yaw, roll)
        , Perspective
        , (100, 50)             -- screen columns × rows
        )
