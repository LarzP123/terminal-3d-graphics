module Matrix where
import Vector

-- | A 4x4 Matrix.
data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4 deriving (Show, Eq)

-- | Transpose a 4x4 matrix 
transposeMat4 :: Mat4 -> Mat4
transposeMat4 (Mat4 (Vec4 r1c1 r1c2 r1c3 r1c4)
                    (Vec4 r2c1 r2c2 r2c3 r2c4)
                    (Vec4 r3c1 r3c2 r3c3 r3c4)
                    (Vec4 r4c1 r4c2 r4c3 r4c4)) =
    Mat4
      (Vec4 r1c1 r2c1 r3c1 r4c1)
      (Vec4 r1c2 r2c2 r3c2 r4c2)
      (Vec4 r1c3 r2c3 r3c3 r4c3)
      (Vec4 r1c4 r2c4 r3c4 r4c4)

instance Semigroup Mat4 where
    (<>) (Mat4 r1 r2 r3 r4) m2 =
        let m2T = transposeMat4 m2
        in Mat4 (multMatVec m2T r1)
                (multMatVec m2T r2)
                (multMatVec m2T r3)
                (multMatVec m2T r4)

instance Monoid Mat4 where
    mempty = Mat4
        (Vec4 1 0 0 0)
        (Vec4 0 1 0 0)
        (Vec4 0 0 1 0)
        (Vec4 0 0 0 1)

-- | Does a 4x4 Matrix times a column Vector of 4 components
multMatVec :: Mat4 -> Vec4 -> Vec4
multMatVec (Mat4 r1 r2 r3 r4) colV = Vec4 (dot r1 colV) (dot r2 colV) (dot r3 colV) (dot r4 colV)

divW :: Vec4 -> Vec4
divW (Vec4 x y z w) = Vec4 (x/w) (y/w) (z/w) w

rotateWorld :: Mat4 -> Vec3 -> Vec3
rotateWorld m (Vec3 a b c) = toVec3 (divW (multMatVec m (Vec4 a b c 1)))

screenPerspectiveMatrix :: (Int, Int) -> Double -> Double -> Mat4
screenPerspectiveMatrix (screenX, screenY) n f =
    let ratio = fromIntegral screenY / fromIntegral screenX
        (right, top) = if screenX > screenY then (1, ratio) else (1/ratio, 1)
    in symmetricPerspectiveMatrix right n top f

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

rotationMatrix :: Vec3 -> Mat4
rotationMatrix (Vec3 pitch yaw roll) = mconcat [pitchMatrix pitch, yawMatrix yaw, rollMatrix roll]
