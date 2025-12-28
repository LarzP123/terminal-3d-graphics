module Matrix where
import Vector

-- | A 4x4 Matrix.
data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4 deriving (Show, Eq)

-- | Multiply two 4x4 matrices
multMatMat :: Mat4 -> Mat4 -> Mat4
multMatMat m1 (Mat4 r1 r2 r3 r4) =
    Mat4
        (multMatVec m1 c1)
        (multMatVec m1 c2)
        (multMatVec m1 c3)
        (multMatVec m1 c4)
  where
    -- columns of the right-hand matrix
    c1 = Vec4 x1 x2 x3 x4
    c2 = Vec4 y1 y2 y3 y4
    c3 = Vec4 z1 z2 z3 z4
    c4 = Vec4 w1 w2 w3 w4
    Vec4 x1 y1 z1 w1 = r1
    Vec4 x2 y2 z2 w2 = r2
    Vec4 x3 y3 z3 w3 = r3
    Vec4 x4 y4 z4 w4 = r4


-- | Does a 4x4 Matrix times a column Vector of 4 components
multMatVec :: Mat4 -> Vec4 -> Vec4
multMatVec (Mat4 r1 r2 r3 r4) colV = Vec4 (dot r1 colV) (dot r2 colV) (dot r3 colV) (dot r4 colV)

divW :: Vec4 -> Vec4
divW (Vec4 x y z w) = Vec4 (x/w) (y/w) (z/w) w

multMatVec3 :: Mat4 -> Vec3 -> Double -> Vec3
multMatVec3 m (Vec3 a b c) w =
    let Vec4 x y z w' = multMatVec m (Vec4 a b c w)
    in Vec3 (x / w') (y / w') (z / w')

-- https://www.mauriciopoppe.com/notes/computer-graphics/viewing/projection-transform/ Eq. 12
symmetricPerspectiveMatrix :: Double -> Double -> Double -> Double -> Mat4
symmetricPerspectiveMatrix r n t f = Mat4
    (Vec4 (n/r) 0     0             0            )
    (Vec4 0     (n/t) 0             0            )
    (Vec4 0     0     ((f+n)/(n-f)) (2*f*n/(n-f)))
    (Vec4 0     0     (-1)            0          )

pitchMatrix :: Double -> Mat4
pitchMatrix a = Mat4
    (Vec4 1       0        0 0)
    (Vec4 0 (cos a) (-sin a) 0)
    (Vec4 0 (sin a)  (cos a) 0)
    (Vec4 0       0        0 1)

yawMatrix :: Double -> Mat4
yawMatrix a = Mat4
    (Vec4  ( cos a) 0 ( sin a) 0)
    (Vec4        0 1       0 0)
    (Vec4 (-sin a) 0 ( cos a) 0)
    (Vec4        0 0       0 1)

rollMatrix :: Double -> Mat4
rollMatrix a = Mat4
    (Vec4 (cos a) (-sin a) 0 0)
    (Vec4 (sin a)  (cos a) 0 0)
    (Vec4       0        0 1 0)
    (Vec4       0        0 0 1)

viewMatrix :: Vec3 -> Mat4
viewMatrix (Vec3 pitch yaw roll) = pitchMatrix pitch  `multMatMat` yawMatrix yaw `multMatMat` rollMatrix roll

