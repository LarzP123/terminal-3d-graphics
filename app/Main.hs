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

-- | A world with a portal on the side of each wall
islandWorld :: IO [Tri Vec3]
islandWorld = do
    [trunkTexture, leafTexture, skyTexture, grassTexture, rockTexture]
            <- mapM readBMP [ "textures/trunk.bmp", "textures/leaf.bmp", "textures/sky.bmp", "textures/grass.bmp", "textures/rock.bmp" ]
    let
        tree = treeFormer (texWallFormer trunkTexture) (texWallFormer leafTexture)
        lights = [Ray (Vec3 0.25 0.25 0.25), Ambient 0.5]
        island = islandFormer 20 30 (texWallFormer rockTexture) (texWallFormer grassTexture)
        shiftedWorld = (fmap . fmap) (+ Vec3 (-5) (-10) (-5)) (island ++ tree)
        room = roomFormer (Vec3 (-100) (-100) (-100)) (Vec3 100 100 100) (texWallFormer skyTexture) (texWallFormer skyTexture)
    pure ((bakeLight lights <$> shiftedWorld) ++ room)

-- | A world with a big castle surrounded by trees on a grassy field
castleWorld :: IO [Tri Vec3]
castleWorld = do
    [trunkTexture, leafTexture, grassTexture]
        <- mapM readBMP [ "textures/trunk.bmp", "textures/leaf.bmp", "textures/grass.bmp" ]
    let rgb r g b = RGB { red = r, green = g, blue = b }
        lights = [Ray (Vec3 0.25 0.25 0.25), Ambient 0.5]
        castle = (fmap . fmap) ((+ Vec3 0 (-30) 250) . (* 3)) (castleFormer (solWallFormer (rgb 150 150 150)) (solWallFormer (rgb 180 30 30)))
        tree = treeFormer (texWallFormer trunkTexture) (texWallFormer leafTexture)
        treeAt (x, z) = (fmap . fmap) ((+ Vec3 x (-14) z) . (* 4)) tree
        trees = concatMap treeAt
            [ (-260, 100), (-160, 100), (160, 100), (260, 100)
            , (-210, 180), (-240, 260), (-210, 340)
            , (210, 180), (240, 260), (210, 340)
            , (-150, 420), (-50, 430), (50, 430), (150, 420)
            ]
        room = roomFormer (Vec3 (-400) (-30) (-100)) (Vec3 400 250 480) (tiledFormer 20 15 (texWallFormer grassTexture)) (solWallFormer (rgb 100 180 250))
    pure ((bakeLight lights <$> (castle ++ trees)) ++ room)

-- | A world on fire: campfires around a brick room with a river of lava running through it
fireWorld :: IO [Tri Vec3]
fireWorld = do
    [logTexture, brickTexture, rockTexture, lavaTexture]
        <- mapM readBMP [ "textures/trunk.bmp", "textures/wall.bmp", "textures/rock.bmp", "textures/lava.bmp" ]
    let rgb r g b = RGB { red = r, green = g, blue = b }
        campfire = fireFormer (texWallFormer logTexture) (solWallFormer (rgb 220 30 0)) (solWallFormer (rgb 255 140 0)) (solWallFormer (rgb 255 230 50))
        fireAt (x, z) = (fmap . fmap) (+ Vec3 x (-30) z) campfire
        fires = concatMap fireAt [ (70, -70), (70, 50), (-85, -65), (-85, 65), (0, 70) ]
        lava = (fmap . fmap) (+ Vec3 0 (-29.5) 0) (lavaFormer (texWallFormer lavaTexture))
        room = roomFormer (Vec3 (-100) (-30) (-100)) (Vec3 100 100 100) (tiledFormer 8 8 (texWallFormer rockTexture)) (tiledFormer 5 3 (texWallFormer brickTexture))
    pure (fires ++ lava ++ room)

-- | A world with a teapot on the floor
teapotWorld :: IO [Tri Vec3]
teapotWorld = do
    [woodTexture, wallpaperTexture] <- mapM readBMP [ "textures/woodFloor.bmp", "textures/wallpaper.bmp" ]
    let rgb r g b = RGB { red = r, green = g, blue = b }
        lights = [Ray (Vec3 0.25 0.25 0.1), Ambient 0.5]
        teapot = (fmap . fmap) (+ Vec3 0 (-30) 30) (teapotFormer (solWallFormer (rgb 240 240 240)) (solWallFormer (rgb 30 90 200)))
        room = roomFormer (Vec3 (-100) (-30) (-100)) (Vec3 100 100 100) (tiledFormer 8 8 (texWallFormer woodTexture)) (tiledFormer 5 3 (texWallFormer wallpaperTexture))
    pure (bakeLight lights <$> (teapot ++ room))

-- | A world with a rainbow over a field
rainbowWorld :: IO [Tri Vec3]
rainbowWorld = do
    [grassTexture, skyTexture] <- mapM readBMP [ "textures/grass.bmp", "textures/sky.bmp" ]
    let rgb r g b = RGB { red = r, green = g, blue = b }
        colours = [ rgb 255 0 0, rgb 255 127 0, rgb 255 255 0, rgb 0 200 0, rgb 0 0 255, rgb 75 0 130, rgb 148 0 211 ]
        rainbow = (fmap . fmap) (+ Vec3 0 (-30) 0) (rainbowFormer (solWallFormer <$> colours))
        room = roomFormer (Vec3 (-100) (-30) (-100)) (Vec3 100 100 100) (tiledFormer 8 8 (texWallFormer grassTexture)) (texWallFormer skyTexture)
    pure ((fmap . fmap) (+ Vec3 0 0 70) (rainbow ++ room))

-- | A list of all of the demo worlds to show off, and a corresponding name
demoWorlds :: [(String, IO [Tri Vec3])]
demoWorlds = [ ("portal", portalWorld), ("chess", chessWorld), ("island", islandWorld), ("castle", castleWorld), ("fire", fireWorld), ("teapot", teapotWorld), ("rainbow", rainbowWorld)]

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