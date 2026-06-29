module Main where

import Terminal3D
import Control.Monad.Trans.State (evalStateT)

-- | A world with a portal on the side of each wall
portalWorld :: IO [Tri Vec3]
portalWorld = do
    [cubeTexture, wallTexture, floorTexture, portalOrangeTexture, portalBlueTexture]
        <- mapM readBMP [ "textures/cube.bmp", "textures/wall.bmp", "textures/floor.bmp", "textures/portalOrange.bmp", "textures/portalBlue.bmp" ]
    let cube = (fmap . fmap) (+ Vec3 20 (-15) 15) (cubeFormer cubeTexture)
        room = roomFormer (Vec3 (-30) (-35) (-60)) (Vec3 70 35 65) floorTexture wallTexture
        portalMin = Vec3 (-28) (-25) (-25)
        portalMax = Vec3   68    20    25
        portal = portalFormer portalBlueTexture portalOrangeTexture
            ( comp3Reduce portalMin portalMin portalMax, comp3Reduce portalMin portalMax portalMin )
            ( comp3Reduce portalMax portalMin portalMax, comp3Reduce portalMax portalMax portalMin )
            False
        lights = [Ray (Vec3 0.25 0.25 0.25), Ambient 0.5]
    pure (bakeLight lights <$> concat [cube, room, portal])

-- | A list of all of the demo worlds to show off, and a corresponding name
demoWorlds :: [(String, IO [Tri Vec3])]
demoWorlds = [ ("portal", portalWorld)]

-- | A function that prompts the user with a list of demo worlds and has them enter a number to choose one. It then returns that demo world
chooseDemoWorld :: IO [Tri Vec3]
chooseDemoWorld = do
    putStrLn "Choose an option:"
    mapM_ putStrLn $ zipWith (++) (map (\n -> show n ++ " - ") ([1..] :: [Int])) (fst <$> demoWorlds)
    readLn >>= \n ->
        if n >= 1 && n <= length demoWorlds
            then snd (demoWorlds !! (n - 1))
            else putStrLn "Invalid choice, try again." >> chooseDemoWorld

-- | Entry point
main :: IO ()
main = do chooseDemoWorld >>= flip (evalStateT . loop) ( def :: AppState )
