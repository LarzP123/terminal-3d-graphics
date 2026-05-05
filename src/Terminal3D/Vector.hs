module Terminal3D.Vector where

-- | A Vector containing 2 components
data Vec2 = Vec2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Eq, Show)

-- | A Vector containing 3 components
data Vec3 = Vec3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Eq, Show)

-- | A Vector containing 4 components
data Vec4 = Vec4 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Eq, Show)

-- Does Z Buffering to get the nearest Vector
instance Ord Vec3 where
    compare (Vec3 _ _ z1) (Vec3 _ _ z2) = compare z1 z2

-- | Vector Cross Product. Creates a new vector perpendicular to the other 2 vectors.
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)

instance Num Vec2 where
    (+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)
    (*) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1*x2) (y1*y2)
    abs = (`vMap` 1) . (*) . magnitude
    signum v = let mag = magnitude v in if mag == 0 then 0 else vMap (/mag) v
    fromInteger = (\n -> Vec2 n n) . fromIntegral
    negate = vMap (*(-1))

instance Num Vec3 where
    (+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)
    (*) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1*x2) (y1*y2) (z1*z2)
    abs = (`vMap` 1) . (*) . magnitude
    signum v = let mag = magnitude v in if mag == 0 then 0 else vMap (/mag) v
    fromInteger = (\n -> Vec3 n n n) . fromIntegral
    negate = vMap (*(-1))

instance Num Vec4 where
    (+) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
    (*) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1*x2) (y1*y2) (z1*z2) (w1*w2)
    abs = (`vMap` 1) . (*) . magnitude
    signum v = let mag = magnitude v in if mag == 0 then 0 else vMap (/mag) v
    fromInteger = (\n -> Vec4 n n n n) . fromIntegral
    negate = vMap (*(-1))

-- | A vector contains a series of doubles
class Num vec => Vector vec where
    magnitude :: vec -> Double
    vMap      :: (Double -> Double) -> vec -> vec
    dot       :: vec -> vec -> Double
    toVec2    :: vec -> Vec2
    toVec3    :: vec -> Vec3
    toVec4    :: vec -> Double -> Vec4
    vF        :: vec -> Double
    vL        :: vec -> Double
    vZ        :: vec -> Double

instance Vector Vec2 where
    magnitude (Vec2 x y)            = sqrt (x*x + y*y)
    vMap f (Vec2 x y)               = Vec2 (f x) (f y)
    dot (Vec2 x1 y1) (Vec2 x2 y2)  = x1*x2 + y1*y2
    toVec2                          = id
    toVec3 (Vec2 x y)               = Vec3 x y 0
    toVec4 (Vec2 x y)               = Vec4 x y 0
    vF (Vec2 x _)                   = x
    vL (Vec2 _ y)                   = y
    vZ (Vec2 _ _)                   = 0

instance Vector Vec3 where
    magnitude (Vec3 x y z)               = sqrt (x*x + y*y + z*z)
    vMap f (Vec3 x y z)                  = Vec3 (f x) (f y) (f z)
    dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
    toVec2 (Vec3 x y _)                  = Vec2 x y
    toVec3                               = id
    toVec4 (Vec3 x y z)                  = Vec4 x y z
    vF (Vec3 x _ _)                      = x
    vL (Vec3 _ _ z)                      = z
    vZ (Vec3 _ _ z)                      = z

-- | Get the Y component of a Vec3
vM :: Vec3 -> Double
vM (Vec3 _ y _) = y

instance Vector Vec4 where
    magnitude (Vec4 x y z w)               = sqrt (x*x + y*y + z*z + w*w)
    vMap f (Vec4 x y z w)                  = Vec4 (f x) (f y) (f z) (f w)
    dot (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2
    toVec2 (Vec4 x y _ _)                  = Vec2 x y
    toVec3 (Vec4 x y z _)                  = Vec3 x y z
    toVec4 v _                             = v
    vF (Vec4 x _ _ _)                      = x
    vL (Vec4 _ _ _ w)                      = w
    vZ (Vec4 _ _ z _)                      = z

-- | Extract one component from each of three values into a Vec3
component3 :: (a -> Double) -> (a, a, a) -> Vec3
component3 f (a, b, c) = Vec3 (f a) (f b) (f c)

-- | Build a Vec3 by taking X from first, Y from second, Z from third
comp3Reduce :: Vec3 -> Vec3 -> Vec3 -> Vec3
comp3Reduce a b c = Vec3 (vF a) (vM b) (vL c)

-- | Weighted sum of three Vec3s using barycentric weights
weight3 :: (Vec3, Vec3, Vec3) -> Vec3 -> Vec3
weight3 (vA, vB, vC) weights =
    vMap (* vF weights) vA + vMap (* vM weights) vB + vMap (* vZ weights) vC
