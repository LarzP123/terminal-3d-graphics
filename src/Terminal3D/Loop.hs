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

-- | (cameraPosition, cameraRotation, projection, screenSize, Supersampling Anti-Aliasing, Post Processing Anti-Aliasing)
type AppState = (Vec3, Vec3, Projection, (Int, Int), AntiAliasing, AntiAliasing)

-- | Parses user input for changing anti-aliasing settings
parseAA :: [String] -> Either String AntiAliasing
parseAA ["box", n]      = case reads n of { [(i, "")] -> Right (aaBox i);      _ -> Left ("Not a valid integer: " ++ n) }
parseAA ["gaussian", n] = case reads n of { [(i, "")] -> Right (aaGaussian i); _ -> Left ("Not a valid integer: " ++ n) }
parseAA _               = Left "Usage: none | box <n> | gaussian <n>"

-- | Main render/input loop.
loop :: [Tri Vec3] -> StateT AppState IO ()
loop world = do
    liftIO $ callCommand "chcp 65001" -- Force UTF8 output on Windows. Hackish
    (currentPos, currentRot, projection, screenSize, ssaa, ppaa) <- get
    liftIO clearScreen
    let rotMat  = rotationMatrix currentRot
        ntcTris = posRotToNtcTris world (currentPos, rotMat)
        textSize = getTextSize screenSize
    liftIO $ LazyByteBuilder.hPut stdout (getScreen ntcTris screenSize projection world rotMat ssaa ppaa)
    liftIO $ printBig textSize ("Position : " ++ show currentPos)
    liftIO $ printBig textSize ("Rotation : " ++ show currentRot)
    liftIO $ printBig textSize ("SSAA     : " ++ show ssaa)
    liftIO $ printBig textSize ("PPAA     : " ++ show ppaa)
    promptLoop world

-- | A loop for prompting the user for what input to do
promptLoop :: [Tri Vec3] -> StateT AppState IO ()
promptLoop world = do
    liftIO $ putStr "Command (or help/quit): "
    liftIO $ hFlush stdout
    cmd <- liftIO getLine
    (currentPos, currentRot, _, screenSize, _, _) <- get
    let textSize = getTextSize screenSize
    case words cmd of
        ("ssaa" : rest) -> case parseAA rest of
            Right newAA  -> modify (\(p, r, pr, s, _, pp) -> (p, r, pr, s, newAA, pp)) >> loop world
            Left err     -> liftIO (printBig textSize err) >> promptLoop world
        ("ppaa" : rest) -> case parseAA rest of
            Right newAA  -> modify (\(p, r, pr, s, sp, _) -> (p, r, pr, s, sp, newAA)) >> loop world
            Left err     -> liftIO (printBig textSize err) >> promptLoop world
        _ -> case cmd of
            "quit" -> liftIO (printBig textSize "Goodbye." >> exitSuccess)
            "?"    -> liftIO (printBig textSize helpText) >> promptLoop world
            "help" -> liftIO (printBig textSize helpText) >> promptLoop world
            _      -> case move cmd currentPos currentRot of
                Nothing       -> liftIO (printBig textSize ("Unknown command: \"" ++ cmd ++ "\". Try '?' for help.")) >> promptLoop world
                Just (p', r') -> modify (\(_, _, pr, s, sp, pp) -> (p', r', pr, s, sp, pp)) >> loop world
