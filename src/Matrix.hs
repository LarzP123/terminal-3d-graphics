module Matrix where
import Vector

-- | A 4x4 Matrix.
data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4 deriving (Show, Eq)

-- | Does a 4x4 Matrix times a column Vector of 4 components
multMatVec :: Mat4 -> Vec4 -> Vec4
multMatVec (Mat4 r1 r2 r3 r4) colV = Vec4 (dot r1 colV) (dot r2 colV) (dot r3 colV) (dot r4 colV)

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

yawMatrix :: Double -> Mat4
yawMatrix a = Mat4
    (Vec4  ( cos a) 0 ( sin a) 0)
    (Vec4        0 1       0 0)
    (Vec4 (-sin a) 0 ( cos a) 0)
    (Vec4        0 0       0 1)

pitchMatrix :: Double -> Mat4
pitchMatrix a = Mat4
    (Vec4 1       0        0 0)
    (Vec4 0 (cos a) (-sin a) 0)
    (Vec4 0 (sin a)  (cos a) 0)
    (Vec4 0       0        0 1)

viewMatrix :: Double -> Mat4
viewMatrix = yawMatrix

