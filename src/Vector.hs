module Vector where
import Numeric

-- | A Vector containing 2 components.
data Vec2 = Vec2 Double Double deriving Eq

-- | A Vector containing 3 components.
data Vec3 = Vec3 Double Double Double deriving Eq

-- | A Vector containing 4 components.
data Vec4 = Vec4 Double Double Double Double deriving Eq

-- Show instances
showVec :: [String] -> [Double] -> String
showVec names values =
    concat $ zipWith (\n v -> n ++ " " ++ showFFloat (Just 3) v "" ++ ", ") names values

instance Show Vec2 where
    show (Vec2 x y) =
        init . init $ showVec ["x","y"] [x,y]

instance Show Vec3 where
    show (Vec3 x y z) =
        init . init $ showVec ["x","y","z"] [x,y,z]

instance Show Vec4 where
    show (Vec4 x y z w) =
        init . init $ showVec ["x","y","z","w"] [x,y,z,w]

-- | Convert Vec4 to Vec3 by dropping the w component
vec4ToVec3 :: Vec4 -> Vec3
vec4ToVec3 (Vec4 x y z _) = Vec3 x y z

-- | Convert Vec4 to Vec2 by dropping z and w
vec4ToVec2 :: Vec4 -> Vec2
vec4ToVec2 (Vec4 x y _ _) = Vec2 x y

-- | Convert Vec3 to Vec2 by dropping z
vec3ToVec2 :: Vec3 -> Vec2
vec3ToVec2 (Vec3 x y _) = Vec2 x y

-- Does Z Buffering to get the nearest Vector
instance Ord Vec3 where
    compare (Vec3 _ _ z1) (Vec3 _ _ z2) = compare z1 z2

-- | Typeclass for vectors that support dot product
class Dot a where
    dot :: a -> a -> Double

instance Dot Vec2 where
    dot (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

instance Dot Vec3 where
    dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

instance Dot Vec4 where
    dot (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2

-- Cross product is only defined for a 3 vector. There is no standard 2 or 4 vector cross product
-- | Vector Cross Product. Creates a new vector perpendicular to the other 2 vectors.
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)

instance Num Vec2 where
    (+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)
    (*) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1*x2) (y1*y2)
    abs (Vec2 x y) = Vec2 mag mag where mag = sqrt (x*x + y*y)
    signum (Vec2 x y) = let mag = sqrt (x*x + y*y) in
        if mag == 0 then Vec2 0 0 else Vec2 (x/mag) (y/mag)
    fromInteger n = let a = fromIntegral n in Vec2 a a
    negate (Vec2 x y) = Vec2 (-x) (-y)

instance Num Vec3 where
    (+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)
    (*) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1*x2) (y1*y2) (z1*z2)
    abs (Vec3 x y z) = Vec3 mag mag mag where mag = sqrt (x*x + y*y + z*z)
    signum (Vec3 x y z) = let mag = sqrt (x*x + y*y + z*z) in
        if mag == 0 then Vec3 0 0 0 else Vec3 (x/mag) (y/mag) (z/mag)
    fromInteger n = let a = fromIntegral n in Vec3 a a a
    negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)

instance Num Vec4 where
    (+) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
    (*) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1*x2) (y1*y2) (z1*z2) (w1*w2)
    abs (Vec4 x y z w) = Vec4 mag mag mag mag where mag = sqrt (x*x + y*y + z*z + w*w)
    signum (Vec4 x y z w) = let mag = sqrt (x*x + y*y + z*z + w*w) in
        if mag == 0 then Vec4 0 0 0 0 else Vec4 (x/mag) (y/mag) (z/mag) (w/mag)
    fromInteger n = let a = fromIntegral n in Vec4 a a a a
    negate (Vec4 x y z w) = Vec4 (-x) (-y) (-z) (-w)

class Magnitude a where
    magnitude :: a -> Double

instance Magnitude Vec2 where
    magnitude (Vec2 x y) = sqrt (x*x + y*y)

instance Magnitude Vec3 where
    magnitude (Vec3 x y z) = sqrt (x*x + y*y + z*z)

instance Magnitude Vec4 where
    magnitude (Vec4 x y z w) = sqrt (x*x + y*y + z*z + w*w)
