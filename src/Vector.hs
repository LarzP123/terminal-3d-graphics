{-# LANGUAGE InstanceSigs #-}
module Vector where

-- | A Vector containing 2 components
data Vec2 = Vec2 Double Double deriving (Eq, Show)

-- | A Vector containing 3 components
data Vec3 = Vec3 Double Double Double deriving (Eq, Show)

-- | A Vector containing 4 components
data Vec4 = Vec4 Double Double Double Double deriving (Eq, Show)

-- Does Z Buffering to get the nearest Vector
instance Ord Vec3 where
    compare :: Vec3 -> Vec3 -> Ordering
    compare (Vec3 _ _ z1) (Vec3 _ _ z2) = compare z1 z2

-- Cross product is only defined for a 3 vector. There is no standard 2 or 4 vector cross product
-- | Vector Cross Product. Creates a new vector perpendicular to the other 2 vectors.
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)

instance Num Vec2 where
    -- Adds component wise
    (+) :: Vec2 -> Vec2 -> Vec2
    (+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)
    -- Multiplies component wise
    (*) :: Vec2 -> Vec2 -> Vec2
    (*) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1*x2) (y1*y2)
    -- Replaces the magnitude of the vector for all components
    abs :: Vec2 -> Vec2
    abs = (.* 1) . magnitude
    -- Normalizes the vector to have magnitude 1. If 0 vector, stays 0 vector
    signum :: Vec2 -> Vec2
    signum v = let mag = magnitude v in
        if mag == 0 then 0 else (1/mag) .* v
    -- Replicates the integer across all components of the vector
    fromInteger :: Integer -> Vec2
    fromInteger = (\n -> Vec2 n n) . fromIntegral
    -- Makes the vector point in the opposite direction
    negate :: Vec2 -> Vec2
    negate = ((-1) .*)

instance Num Vec3 where
    -- Adds component wise
    (+) :: Vec3 -> Vec3 -> Vec3
    (+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)
    -- Multiplies component wise
    (*) :: Vec3 -> Vec3 -> Vec3
    (*) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1*x2) (y1*y2) (z1*z2)
    -- Replaces the magnitude of the vector for all components
    abs :: Vec3 -> Vec3
    abs = (.* 1) . magnitude
    -- Normalizes the vector to have magnitude 1. If 0 vector, stays 0 vector
    signum :: Vec3 -> Vec3
    signum v = let mag = magnitude v in
        if mag == 0 then 0 else (1/mag) .* v
    -- Replicates the integer across all components of the vector
    fromInteger :: Integer -> Vec3
    fromInteger = (\n -> Vec3 n n n) . fromIntegral
    -- Makes the vector point in the opposite direction
    negate :: Vec3 -> Vec3
    negate = ((-1) .*)

instance Num Vec4 where
    -- Adds component wise
    (+) :: Vec4 -> Vec4 -> Vec4
    (+) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
    -- Multiplies component wise
    (*) :: Vec4 -> Vec4 -> Vec4
    (*) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1*x2) (y1*y2) (z1*z2) (w1*w2)
    -- Replaces the magnitude of the vector for all components
    abs :: Vec4 -> Vec4
    abs = (.* 1) . magnitude
    -- Normalizes the vector to have magnitude 1. If 0 vector, stays 0 vector
    signum :: Vec4 -> Vec4
    signum v = let mag = magnitude v in
        if mag == 0 then 0 else (1/mag) .* v
    -- Replicates the integer across all components of the vector
    fromInteger :: Integer -> Vec4
    fromInteger = (\n -> Vec4 n n n n) . fromIntegral
    -- Makes the vector point in the opposite direction
    negate :: Vec4 -> Vec4
    negate = ((-1) .*)

-- | A vector contains a series of doubles
class Vector vec where
    -- | Returns back a double given the magnitude of the vector in n-dimensional space
    magnitude :: vec -> Double
    -- | Multiplies a scalar by the vector (Multiplying all components of the vector by the scalar)
    scalarMult :: Double -> vec -> vec
    -- | Does a dot product with the vector
    dot :: vec -> vec -> Double
    -- | Converts the vector to a 2 vector. May loose information in the process
    toVec2 :: vec -> Vec2
    -- | Converts the vector to a 3 vector. May loose information or gain 0s in the process
    toVec3 :: vec -> Vec3
    -- | Gets first component
    vF :: vec -> Double
    -- | Gets last component
    vL :: vec -> Double

instance Vector Vec2 where
    magnitude :: Vec2 -> Double
    magnitude (Vec2 x y) = sqrt (x*x + y*y)
    scalarMult :: Double -> Vec2 -> Vec2
    scalarMult scalar = (Vec2 scalar scalar *)
    dot :: Vec2 -> Vec2 -> Double
    dot (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2
    toVec2 :: Vec2 -> Vec2
    toVec2 = id
    toVec3 :: Vec2 -> Vec3
    toVec3 (Vec2 x y) = Vec3 x y 0
    vF :: Vec2 -> Double
    vF (Vec2 x _) = x
    vL :: Vec2 -> Double
    vL (Vec2 _ y) = y

instance Vector Vec3 where
    magnitude :: Vec3 -> Double
    magnitude (Vec3 x y z) = sqrt (x*x + y*y + z*z)
    scalarMult :: Double -> Vec3 -> Vec3
    scalarMult scalar = (Vec3 scalar scalar scalar *)
    dot :: Vec3 -> Vec3 -> Double
    dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
    toVec2 :: Vec3 -> Vec2
    toVec2 (Vec3 x y _) = Vec2 x y
    toVec3 :: Vec3 -> Vec3
    toVec3 = id
    vF :: Vec3 -> Double
    vF (Vec3 x _ _) = x
    vL :: Vec3 -> Double
    vL (Vec3 _ _ z) = z

vM :: Vec3 -> Double
vM (Vec3 _ y _) = y

instance Vector Vec4 where
    magnitude :: Vec4 -> Double
    magnitude (Vec4 x y z w) = sqrt (x*x + y*y + z*z + w*w)
    scalarMult :: Double -> Vec4 -> Vec4
    scalarMult scalar = (Vec4 scalar scalar scalar scalar *)
    dot :: Vec4 -> Vec4 -> Double
    dot (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2
    toVec2 :: Vec4 -> Vec2
    toVec2 (Vec4 x y _ _) = Vec2 x y
    toVec3 :: Vec4 -> Vec3
    toVec3 (Vec4 x y z _) = Vec3 x y z
    vF :: Vec4 -> Double
    vF (Vec4 x _ _ _) = x
    vL :: Vec4 -> Double
    vL (Vec4 _ _ _ w) = w

-- | Scalar multiplication
infixl 7 .*  -- left-associative, precedence similar to *
(.*) :: Vector a => Double -> a -> a
(.*) = scalarMult

-- For Vec2 interpolation
component2 :: (a -> Double) -> a -> a -> Vec2
component2 f a b = Vec2 (f a) (f b)

comp2Reduce :: Vec2 -> Vec2 -> Vec2
comp2Reduce a b = Vec2 (vF a) (vL b)

-- For Vec3 interpolation
component3 :: (a -> Double) -> a -> a -> a -> Vec3
component3 f a b c = Vec3 (f a) (f b) (f c)

comp3Reduce :: Vec3 -> Vec3 -> Vec3 -> Vec3
comp3Reduce a b c = Vec3 (vF a) (vM b) (vL c)

-- For Vec4 interpolation
component4 :: (a -> Double) -> a -> a -> a -> a -> Vec4
component4 f a b c d = Vec4 (f a) (f b) (f c) (f d)

comp4Reduce :: Vec4 -> Vec4 -> Vec4 -> Vec4 -> Vec4
comp4Reduce a (Vec4 _ bY _ _) (Vec4 _ _ cZ _) d = Vec4 (vF a) bY cZ (vL d)
