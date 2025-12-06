import Vector
import Data.Maybe (isJust)

-- | A 4x4 Matrix.
data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4 deriving (Show, Eq)

-- https://www.mauriciopoppe.com/notes/computer-graphics/viewing/projection-transform/ Eq. 12
symmetricPerspectiveMatrix :: Double -> Double -> Double -> Double -> Mat4
symmetricPerspectiveMatrix r n t f = Mat4
    (Vec4 (n/r) 0     0             0            )
    (Vec4 0     (n/t) 0             0            )
    (Vec4 0     0     ((f+n)/(n-f)) (2*f*n/(n-f)))
    (Vec4 0     0     (-1)            0          )

-- | A Vector containing 4 components.
data Tri = Tri Vec4 Vec4 Vec4 deriving Show

-- | barycentric-coordinate point-in-triangle test + depth interpolation
pointInsideTriDepth :: (Double, Double) -> Tri -> Maybe Double
pointInsideTriDepth (px, py)
    (Tri p1@(Vec4 _ _ z1 _)
         p2@(Vec4 _ _ z2 _)
         p3@(Vec4 _ _ z3 _)) =
    let
        -- Convert point to Vec4
        p  = Vec4 px py 0 0

        -- Edge vectors
        v0 = p3 - p1
        v1 = p2 - p1
        v2 = p  - p1

        -- Dot products
        dot00 = dot v0 v0
        dot01 = dot v0 v1
        dot02 = dot v0 v2
        dot11 = dot v1 v1
        dot12 = dot v1 v2

        denom = dot00 * dot11 - dot01 * dot01

        u = (dot11 * dot02 - dot01 * dot12) / denom
        v = (dot00 * dot12 - dot01 * dot02) / denom
        w = 1 - u - v
    in
        if u >= 0 && v >= 0 && w >= 0
            then Just (w*z1 + v*z2 + u*z3)
            else Nothing

-- | Does a 4x4 Matrix times a column Vector of 4 components
multMatVec :: Mat4 -> Vec4 -> Vec4
multMatVec (Mat4 r1 r2 r3 r4) colV = Vec4 (dot r1 colV) (dot r2 colV) (dot r3 colV) (dot r4 colV)

-- | Gets a 3d perspective to display to the screen given 2d tris to display
getScreen :: [Tri] -> Int -> String
getScreen tris screenSize =
    unlines
        [ [ if any (isJust . pointInsideTriDepth
                     ( (fromIntegral x / (2 * fromIntegral screenSize)) - 0.5
                     , (fromIntegral y / fromIntegral screenSize) - 0.5
                     )
                  ) tris
                then '#'
                else ' '
          | x <- [0..2*screenSize]
          ]
        | y <- [0..screenSize]
        ]

-- | TEMP TEMP TEMP
testTri :: Tri
testTri =
    Tri
        (Vec4 (-50) 2 (-4.0) 1)
        (Vec4 25 8 (-4.0) 1)
        (Vec4 10 50 (-4.0) 1)

-- | Clears the terminal
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Convert 3D space point to screen coordinates
spacePointToScreen :: Mat4 -> Vec4 -> Vec4
spacePointToScreen perspectiveMat coord4D =
    let Vec4 rx ry rz rw = multMatVec perspectiveMat coord4D
    in Vec4 (rx / rw) (ry / rw) (rz / rw) 1.0

-- | Converts a series of 3d triangles to 2d triangles given a camera perspective
get2DTris :: Mat4 -> [Tri] -> [Tri]
get2DTris perspectiveMat = map (\(Tri a b c) -> Tri (spacePointToScreen perspectiveMat a)
            (spacePointToScreen perspectiveMat b)
            (spacePointToScreen perspectiveMat c))

-- String input to move the camera. Takes in string input and the current position to output the new position
move :: String -> Vec4 -> Vec4
move cmd (Vec4 x y z w) =
    case cmd of
        "forward"  -> Vec4 (x + 1) y z w
        "backward" -> Vec4 (x - 1) y z w
        _          -> Vec4 x y z w

-- | Triangle minus a vector coordiante. Just shifts the triangle around
triSubVec :: Tri -> Vec4 -> Tri
triSubVec (Tri a b c) v = Tri (a-v) (b-v) (c-v)

-- | Recursive loop for each "frame" of the 3d game
loop :: Vec4 -> IO ()
loop currentPos = do
    clearScreen
    -- print current screen
    let screenMat = symmetricPerspectiveMatrix 2 0.1 2 10
    let relativeTris = [triSubVec testTri currentPos]
    let screenTris = get2DTris screenMat relativeTris
    putStrLn (getScreen screenTris 10)
    -- print current pos
    putStrLn $ "Current position: " ++ show currentPos
    putStrLn "Enter command (forward/backward/quit): "
    cmd <- getLine
    if cmd == "quit"
        then putStrLn "Exiting."
        else loop (move cmd currentPos)

-- Entry point
main :: IO ()
main = do
    loop (Vec4 0 0 0 0)

