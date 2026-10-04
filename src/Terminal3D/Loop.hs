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
import Terminal3D.Localization
import Data.Char
import Data.List

-- | (cameraPosition, cameraRotation, projection, screenSize, Supersampling Anti-Aliasing, Post Processing Anti-Aliasing)
newtype AppState = AppState (Vec3, Vec3, Projection, (Int, Int), AntiAliasing, AntiAliasing, Language)

instance Default AppState where
    def = AppState ( 0, 0, Perspective, (100, 50), def, def, langEnglish )


instance Show AppState where
    show (AppState (pos, rot, proj, screenSize, ssaa, ppaa, language)) =
        unlines (map showProperty properties)
        where
            translator :: (Local -> String)
            translator = langPick language
            showProperty :: (String, String) -> String 
            showProperty (label, value) = label ++ concat (replicate (spacesToColon - length label) " ") ++ ": " ++ value
            properties :: [(String, String)]
            properties = [
                (translator appstatePosition, show pos),
                (translator appstateRotation, show rot),
                (translator appstateProjection, displayProjection translator proj),
                (translator appstateScreenSize, show screenSize),
                (translator appstateSSAA, dispalyAntiAliasing translator ssaa),
                (translator appstatePPAA, dispalyAntiAliasing translator ppaa),
                (translator appstateLanguage, translator $ langName language)
                ]
            spacesToColon :: Int
            spacesToColon = maximum $ map (length . fst) properties

-- | Parses user input for changing anti-aliasing settings
parseAA :: [String] -> (Local -> String) -> Either String AntiAliasing
parseAA [kind, n] translator
    | kind == translator inputTypeBox      = case reads n of { [(i, "")] -> Right (aaBox i);      _ -> Left (translator feedbackInteger ++ ": " ++ n) }
    | kind == translator inputTypeGaussian = case reads n of { [(i, "")] -> Right (aaGaussian i); _ -> Left (translator feedbackInteger ++ ": " ++ n) }
parseAA _ translator = Left $ translator feedbackUsage ++ ": " ++ translator inputTypeNone ++ " | " ++ translator inputTypeBox ++ " <n> | " ++ translator inputTypeGaussian ++ " <n>"

-- | Parses user input for changing the screen size
parseSize :: [String] -> (Local -> String) -> Either String (Int, Int)
parseSize [w, h] translator = case (reads w, reads h) of
    ([(w', "")], [(h', "")]) | w' > 0 && h' > 0 -> Right (w', h')
    _ -> Left $ translator feedbackPositiveIntegers
parseSize _ translator = Left $ translator feedbackUsage ++ ":" ++ " " ++ translator appstateScreenSize ++ "  <" ++ translator inputTypeWidth ++ "> <" ++ translator inputTypeHeight ++ ">"

-- | Parses user input for changing the screen size
parseLang :: [String] -> (Local -> String) -> Either String Language
parseLang args translator = case args of
    [lang] | Just l <- find (matches lang) allLanguages -> Right l
    _ -> Left $ translator feedbackUsage ++ ": " ++ translator appstateLanguage
                ++ " <" ++ intercalate " | " (map (translator . langName) allLanguages) ++ ">"
    where
        matches lang l = map toLower (translator (langName l)) == map toLower lang

-- | Return a help string listing all available commands.
helpText :: (Local -> String) -> String
helpText translator = 
    let aaFields = [translator appstateSSAA, translator appstatePPAA]
    in unlines $
        map (\(MoveOperation _ _ c name) -> "  " ++ [c] ++ "  " ++ translator name) moveOperations ++
        [ unwords (map (((label ++ " ") ++ ) . (++ " <n>") . translator . aaName . ($ 0)) aaMethods) | label <- aaFields ] ++
        [ translator appstateScreenSize ++ " <w> <h>" ]

-- | A world's triangles plus optional walls the camera can't leave
data World = World
    { worldTris   :: [Tri Vec3]
    , worldBounds :: Maybe Bounds
    }

-- | Main render/input loop.
loop :: World -> StateT AppState IO ()
loop world = do
    liftIO $ callCommand "chcp 65001" -- Force UTF8 output on Windows. Hackish
    appState@(AppState (currentPos, currentRot, projection, screenSize, ssaa, ppaa, _)) <- get
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
    AppState (currentPos, currentRot, _, screenSize, _, _, language) <- get
    let textSize = getTextSize screenSize
        translator = langPick language
    liftIO $ putStr (translator feedbackStartText ++ ": ")
    liftIO $ hFlush stdout
    cmd <- liftIO getLine
    case words cmd of
        (ssaaWord : rest) | ssaaWord == translator appstateSSAA -> case parseAA rest translator of
            Right newAA  -> modify (\(AppState (p, r, pr, s, _, pp, lang)) -> AppState (p, r, pr, s, newAA, pp, lang)) >> loop world
            Left err     -> liftIO (printBig textSize err) >> promptLoop world
        (ppaaWord : rest) | ppaaWord == translator appstatePPAA -> case parseAA rest translator of
            Right newAA  -> modify (\(AppState(p, r, pr, s, sp, _, lang)) -> AppState (p, r, pr, s, sp, newAA, lang)) >> loop world
            Left err     -> liftIO (printBig textSize err) >> promptLoop world
        (screenSizeWord : rest) | screenSizeWord == translator appstateScreenSize -> case parseSize rest translator of
            Right newSize -> modify (\(AppState (p, r, pr, _, sp, pp, lang)) -> AppState (p, r, pr, newSize, sp, pp, lang)) >> loop world
            Left err      -> liftIO (printBig textSize err) >> promptLoop world
        (langWord : rest) | langWord == translator appstateLanguage -> case parseLang rest translator of
            Right newLang -> modify (\(AppState (p, r, pr, s, sp, pp, _)) -> AppState (p, r, pr, s, sp, pp, newLang)) >> loop world
            Left err      -> liftIO (printBig textSize err) >> promptLoop world
        _ -> case cmd of
            _ | cmd == translator commandQuit  -> liftIO (printBig textSize (translator feedbackGoodbye) >> exitSuccess)
            "?" -> liftIO (printBig textSize (helpText translator)) >> promptLoop world
            _ | cmd == translator commandHelp -> liftIO (printBig textSize (helpText translator)) >> promptLoop world
            _ -> case move (worldBounds world) cmd currentPos currentRot translator of
                Nothing       -> liftIO (printBig textSize (translator feedbackUnknownCommand ++ ": \"" ++ cmd ++ "\". " ++ translator feedbackHelpUnknownCommand)) >> promptLoop world
                Just (p', r') -> modify (\(AppState (_, _, pr, s, sp, pp, lang)) -> AppState (p', r', pr, s, sp, pp, lang)) >> loop world
