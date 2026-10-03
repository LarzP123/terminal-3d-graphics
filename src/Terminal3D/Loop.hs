module Terminal3D.Loop where

import Terminal3D.Vector
import Terminal3D.Tri

import Control.Monad.Trans.State
import Terminal3D.TerminalGraphics
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LazyByteBuilder
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Terminal3D.Matrix ( rotationMatrix )
import Terminal3D.Movement
import Terminal3D.BigText
import System.Process (callCommand)
import Data.List

-- | (cameraPosition, cameraRotation, projection, screenSize, Supersampling Anti-Aliasing, Post Processing Anti-Aliasing)
newtype AppState = AppState (Vec3, Vec3, Projection, (Int, Int), AntiAliasing, AntiAliasing)

instance Default AppState where
    def = AppState ( 0, 0, Perspective, (100, 50), def, def )

instance Show AppState where
    show (AppState (pos, rot, proj, screenSize, ssaa, ppaa)) =
        unlines
            [ "Position  : " ++ show pos
            , "Rotation  : " ++ show rot
            , "Projection: " ++ show proj
            , "ScreenSize: " ++ show screenSize
            , "SSAA      : " ++ show ssaa
            , "PPAA      : " ++ show ppaa
            ]

-- | Parses user input for changing anti-aliasing settings
parseAA :: [String] -> Either String AntiAliasing
parseAA ["box", n]      = case reads n of { [(i, "")] -> Right (aaBox i);      _ -> Left ("Not a valid integer: " ++ n) }
parseAA ["gaussian", n] = case reads n of { [(i, "")] -> Right (aaGaussian i); _ -> Left ("Not a valid integer: " ++ n) }
parseAA _               = Left "Usage: none | box <n> | gaussian <n>"

-- | Parses user input for changing the screen size
parseSize :: [String] -> Either String (Int, Int)
parseSize [w, h] = case (reads w, reads h) of
    ([(w', "")], [(h', "")]) | w' > 0 && h' > 0 -> Right (w', h')
    _ -> Left "Width and height must be positive integers"
parseSize _ = Left "Usage: ScreenSize <width> <height>"

-- | Return a help string listing all available commands.
helpText :: String
helpText = 
    let aaFields = filter ("AA" `isSuffixOf`) (map (takeWhile (/= ' ')) (lines (show (def :: AppState))))
    in unlines $
        map (\(MoveOperation _ _ c name) -> "  " ++ [c] ++ "  " ++ name) moveOperations ++
        [ unwords (map (((label ++ " ") ++ ) . (++ " <n>") . aaName . ($ 0)) aaMethods) | label <- aaFields ] ++
        [ "ScreenSize <w> <h>" ]

-- | A world's triangles plus optional walls the camera can't leave
data World = World
    { worldTris   :: [Tri Vec3]
    , worldBounds :: Maybe Bounds
    }

-- | Main render/input loop.
loop :: World -> StateT AppState IO ()
loop world = do
    liftIO $ callCommand "chcp 65001" -- Force UTF8 output on Windows. Hackish
    appState@(AppState (currentPos, currentRot, projection, screenSize, ssaa, ppaa)) <- get
    liftIO clearScreen
    let tris     = worldTris world
        rotMat   = rotationMatrix currentRot
        ntcTris  = posRotToNtcTris tris (currentPos, rotMat)
        textSize = getTextSize screenSize
    liftIO $ LazyByteBuilder.hPut stdout (getScreen ntcTris screenSize projection tris rotMat ssaa ppaa)
    liftIO $ printBig textSize (show appState)
    promptLoop world

-- | A loop for prompting the user for what input to do
promptLoop :: World -> StateT AppState IO ()
promptLoop world = do
    liftIO $ putStr "Command (or help/quit): "
    liftIO $ hFlush stdout
    cmd <- liftIO getLine
    AppState (currentPos, currentRot, _, screenSize, _, _) <- get
    let textSize = getTextSize screenSize
    case words cmd of
        ("ssaa" : rest) -> case parseAA rest of
            Right newAA  -> modify (\(AppState (p, r, pr, s, _, pp)) -> AppState (p, r, pr, s, newAA, pp)) >> loop world
            Left err     -> liftIO (printBig textSize err) >> promptLoop world
        ("ppaa" : rest) -> case parseAA rest of
            Right newAA  -> modify (\(AppState(p, r, pr, s, sp, _)) -> AppState (p, r, pr, s, sp, newAA)) >> loop world
            Left err     -> liftIO (printBig textSize err) >> promptLoop world
        ("screensize" : rest) -> case parseSize rest of
            Right newSize -> modify (\(AppState (p, r, pr, _, sp, pp)) -> AppState (p, r, pr, newSize, sp, pp)) >> loop world
            Left err      -> liftIO (printBig textSize err) >> promptLoop world
        _ -> case cmd of
            "quit" -> liftIO (printBig textSize "Goodbye." >> exitSuccess)
            "?"    -> liftIO (printBig textSize helpText) >> promptLoop world
            "help" -> liftIO (printBig textSize helpText) >> promptLoop world
            _      -> case move (worldBounds world) cmd currentPos currentRot of   -- CHANGED (added worldBounds world)
                Nothing       -> liftIO (printBig textSize ("Unknown command: \"" ++ cmd ++ "\". Try '?' for help.")) >> promptLoop world
                Just (p', r') -> modify (\(AppState (_, _, pr, s, sp, pp)) -> AppState (p', r', pr, s, sp, pp)) >> loop world
