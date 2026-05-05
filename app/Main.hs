module Main where

import Terminal3D
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)
import Data.List (find)
import System.IO (hFlush, stdout)

{-| A possible movement operation containning a position transform (rotation -> position -> output position)
    , rotation transform, action character, and full name -}
data MoveOperation = MoveOperation (Vec3 -> Vec3 -> Vec3) (Vec3 -> Vec3) Char String

moveOperations :: [MoveOperation]
moveOperations =
    [
        MoveOperation (const id) id 'n' "do nothing",
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x - speed * sin yaw) y (z + speed * cos yaw)) id 'w' "move forward",
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x + speed * sin yaw) y (z - speed * cos yaw)) id 's' "move backward",
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x - speed * cos yaw) y (z - speed * sin yaw)) id 'd' "strafe right",
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x + speed * cos yaw) y (z + speed * sin yaw)) id 'a' "strafe left",
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 p (y - yawInc) r)   'j' "turn left",
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 p (y + yawInc) r)   'l' "turn right",
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 (p + pitchInc) y r) 'i' "turn up",
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 (p - pitchInc) y r) 'k' "turn down"
    ]
  where
    speed    = 5
    pitchInc = 0.2
    yawInc   = 0.2

-- | Parse a movement command and return updated (position, rotation), or Nothing if invalid.
move :: String -> Vec3 -> Vec3 -> Maybe (Vec3, Vec3)
move cmd pos rot =
    case find (\(MoveOperation _ _ c name) -> name == cmd || [c] == cmd) moveOperations of
        Just (MoveOperation posT rotT _ _) -> Just (posT rot pos, rotT rot)
        Nothing                            -> Nothing

-- | Return a help string listing all available commands.
helpText :: String
helpText = unlines $
    map (\(MoveOperation _ _ c name) -> "  " ++ [c] ++ "  " ++ name) moveOperations
    ++
    [ -- Temporary hardcoding of command help text.
        "",
        "Anti-aliasing (post-process):    ppaa box <n> | ppaa gaussian <n>",
        "Anti-aliasing (supersampling):   ssaa box <n> | ssaa gaussian <n>"
    ]

-- | (cameraPosition, cameraRotation, projection, screenSize, Supersampling Anti-Aliasing, Post Processing Anti-Aliasing)
type AppState = (Vec3, Vec3, Projection, (Int, Int), AntiAliasing, AntiAliasing)

-- | Main render/input loop.
loop :: [Tri Vec3] -> StateT AppState IO ()
loop world = do
    (currentPos, currentRot, projection, screenSize, ssaa, ppaa) <- get
    liftIO clearScreen
    let rotMat  = rotationMatrix currentRot
        ntcTris = posRotToNtcTris world (currentPos, rotMat)
    liftIO $ putStrLn (getScreen ntcTris screenSize projection world rotMat ssaa ppaa)
    liftIO $ putStrLn ("Position : " ++ show currentPos)
    liftIO $ putStrLn ("Rotation : " ++ show currentRot)
    liftIO $ putStrLn ("SSAA     : " ++ show ssaa)
    liftIO $ putStrLn ("PPAA     : " ++ show ppaa)
    promptLoop world

promptLoop :: [Tri Vec3] -> StateT AppState IO ()
promptLoop world = do
    liftIO $ putStr "Command (or help/quit): "
    liftIO $ hFlush stdout
    cmd <- liftIO getLine
    (currentPos, currentRot, _, _, _, _) <- get
    case words cmd of
        ("ssaa" : rest) -> case parseAA rest of
            Right newAA  -> modify (\(p, r, pr, s, _, pp) -> (p, r, pr, s, newAA, pp)) >> loop world
            Left err     -> liftIO (putStrLn err) >> promptLoop world
        ("ppaa" : rest) -> case parseAA rest of
            Right newAA  -> modify (\(p, r, pr, s, sp, _) -> (p, r, pr, s, sp, newAA)) >> loop world
            Left err     -> liftIO (putStrLn err) >> promptLoop world
        _ -> case cmd of
            "quit" -> liftIO (putStrLn "Goodbye." >> exitSuccess)
            "?"    -> liftIO (putStrLn helpText) >> promptLoop world
            "help" -> liftIO (putStrLn helpText) >> promptLoop world
            _      -> case move cmd currentPos currentRot of
                Nothing       -> liftIO (putStrLn ("Unknown command: \"" ++ cmd ++ "\". Try '?' for help.")) >> promptLoop world
                Just (p', r') -> modify (\(_, _, pr, s, sp, pp) -> (p', r', pr, s, sp, pp)) >> loop world

-- | Parses user input for changing anti-aliasing settings
parseAA :: [String] -> Either String AntiAliasing
parseAA ["box", n]      = case reads n of { [(i, "")] -> Right (aaBox i);      _ -> Left ("Not a valid integer: " ++ n) }
parseAA ["gaussian", n] = case reads n of { [(i, "")] -> Right (aaGaussian i); _ -> Left ("Not a valid integer: " ++ n) }
parseAA _               = Left "Usage: none | box <n> | gaussian <n>"

{-| Build the demo scene: a lit room containing a cube and two portals.
texture paths are resolved relative to the working directory at runtime. -}
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

        lights = [Ray (Vec3 0.25 0.25 0.25), Ambient 0.5]
        world  = cube ++ roomFloor ++ wallFront ++ wallBack
                    ++ wallLeft  ++ wallRight  ++ portal

    return (fmap (bakeLight lights) world)

-- | Entry point
main :: IO ()
main = do
    world <- createWorld
    evalStateT (loop world)
        ( Vec3 (-20) 15 (-15)
        , Vec3 0.0 (-1.2) 0
        , Perspective
        , (1000, 500)
        , aaBox 1 -- supersampling
        , aaBox 1 -- post-process
        )
