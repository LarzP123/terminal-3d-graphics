{-# LANGUAGE InstanceSigs #-}
module Vector where

-- | A Vector containing 3 components.
data Vec3 = Vec3 Double Double Double deriving (Show, Eq)

-- | A Vector containing 4 components.
data Vec4 = Vec4 Double Double Double Double deriving (Show, Eq)

-- Does Z Buffering to get the nearest Vector
instance Ord Vec3 where
    compare :: Vec3 -> Vec3 -> Ordering
    compare (Vec3 _ _ z1) (Vec3 _ _ z2) = compare z1 z2

-- | Vector Dot Product. Multiplies corresponding components and then takes a sum.
class Dot a where
    dot :: a -> a -> Double

instance Dot Vec3 where
    dot :: Vec3 -> Vec3 -> Double
    dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

instance Dot Vec4 where
    dot :: Vec4 -> Vec4 -> Double
    dot (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2

-- | Vector Cross Product. Creates a new vector perpendicular to the other 2 vectors.
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)

-- Cross product is only defined for a 3 vector. There is no standard 4 vector cross product

instance Num Vec3 where
    (+) :: Vec3 -> Vec3 -> Vec3
    (+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)
    -- Componentwise multiplication. Neither dot nor cross product.
    (*) :: Vec3 -> Vec3 -> Vec3
    (*) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1*x2) (y1*y2) (z1*z2)
    -- | vector magnitude across all components.
    abs :: Vec3 -> Vec3
    abs (Vec3 x y z) = Vec3 mag mag mag
        where mag = sqrt (x*x + y*y + z*z)
    -- | Returns back a unit Vector in the same direction as the original vector.
    signum :: Vec3 -> Vec3
    signum (Vec3 x y z)
        | mag == 0  = Vec3 0 0 0
        | otherwise = Vec3 (x/mag) (y/mag) (z/mag)
        where mag = sqrt (x*x + y*y + z*z)
    fromInteger :: Integer -> Vec3
    fromInteger a = Vec3 n n n
        where n = fromIntegral a
    negate :: Vec3 -> Vec3
    negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)

instance Num Vec4 where
    (+) :: Vec4 -> Vec4 -> Vec4
    (+) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
    -- Componentwise multiplication. Neither dot nor cross product.
    (*) :: Vec4 -> Vec4 -> Vec4
    (*) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1*x2) (y1*y2) (z1*z2) (w1+w2)
    -- | vector magnitude across all components.
    abs :: Vec4 -> Vec4
    abs (Vec4 x y z w) = Vec4 mag mag mag mag
        where mag = sqrt (x*x + y*y + z*z + w*w)
    -- | Returns back a unit Vector in the same direction as the original vector.
    signum :: Vec4 -> Vec4
    signum (Vec4 x y z w)
        | mag == 0  = Vec4 0 0 0 0
        | otherwise = Vec4 (x/mag) (y/mag) (z/mag) (w/mag)
        where mag = sqrt (x*x + y*y + z*z + w*w)
    fromInteger :: Integer -> Vec4
    fromInteger a = Vec4 n n n n
        where n = fromIntegral a
    negate :: Vec4 -> Vec4
    negate (Vec4 x y z w) = Vec4 (-x) (-y) (-z) (-w)

