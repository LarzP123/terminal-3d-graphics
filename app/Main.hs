module Main where

import Terminal3D
import Control.Monad.Trans.State (evalStateT)

-- | A world with a portal on the side of each wall
portalWorld :: IO [Tri Vec3]
portalWorld = do
    [cubeTexture, wallTexture, floorTexture, portalOrangeTexture, portalBlueTexture]
        <- mapM readBMP [ "textures/cube.bmp", "textures/wall.bmp", "textures/floor.bmp", "textures/portalOrange.bmp", "textures/portalBlue.bmp" ]
    let cube = (fmap . fmap) (+ Vec3 20 (-15) 15) (cubeFormer (texWallFormer cubeTexture))
        room = roomFormer (Vec3 (-30) (-35) (-60)) (Vec3 70 35 65) (texWallFormer floorTexture) (texWallFormer wallTexture)
        portalMin = Vec3 (-28) (-25) (-25)
        portalMax = Vec3   68    20    25
        portal = portalFormer (texWallFormer portalBlueTexture) (texWallFormer portalOrangeTexture)
            ( comp3Reduce portalMin portalMin portalMax, comp3Reduce portalMin portalMax portalMin )
            ( comp3Reduce portalMax portalMin portalMax, comp3Reduce portalMax portalMax portalMin )
            False
        lights = [Ray (Vec3 0.25 0.25 0.25), Ambient 0.5]
    pure (bakeLight lights <$> concat [cube, room, portal])

-- | A world with a portal on the side of each wall
chessWorld :: IO [Tri Vec3]
chessWorld = do
    let tileTris i j =
            let fi = fromIntegral i
                fj = fromIntegral j
                (v0, v1, v2, v3) = ( Vec3 fi 0 fj, Vec3 (fi + 1) 0 fj, Vec3 (fi + 1) 0 (fj + 1), Vec3 fi 0 (fj + 1) )
                col = if even (i + j) then Solid RGB { red = 250, green = 250, blue = 250 }
                    else Solid RGB { red = 0, green = 0, blue = 0 }
            in [ Tri v0 v1 v2 col, Tri v0 v2 v3 col ]
        board = concat [ tileTris i j | i <- [0 :: Int .. 8], j <- [0 :: Int .. 8] ]
        movedBoard = (fmap . fmap) ((+ Vec3 (-5) (-10) (-5)) . (* 5)) board
        skyBlue = RGB { red = 0, green = 0, blue = 250 }
        room = roomFormer (Vec3 (-100) (-100) (-100)) (Vec3 100 100 100) (solWallFormer skyBlue) (solWallFormer skyBlue)
    pure ((bakeLight [Ambient 0.5] <$> movedBoard) ++ room)

-- | A list of all of the demo worlds to show off, and a corresponding name
demoWorlds :: [(String, IO [Tri Vec3])]
demoWorlds = [ ("portal", portalWorld), ("chess", chessWorld)]

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
