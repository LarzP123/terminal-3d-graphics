{-# LANGUAGE InstanceSigs #-}
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
    -- Multiply two 4x4 matrices
    (<>) :: Mat4 -> Mat4 -> Mat4
    (<>) (Mat4 (Vec4 a11 a12 a13 a14)
                (Vec4 a21 a22 a23 a24)
                (Vec4 a31 a32 a33 a34)
                (Vec4 a41 a42 a43 a44))
        (Mat4 (Vec4 b11 b12 b13 b14)
                (Vec4 b21 b22 b23 b24)
                (Vec4 b31 b32 b33 b34)
                (Vec4 b41 b42 b43 b44)) =
        let r1 = Vec4 (a11*b11 + a12*b21 + a13*b31 + a14*b41)
                    (a11*b12 + a12*b22 + a13*b32 + a14*b42)
                    (a11*b13 + a12*b23 + a13*b33 + a14*b43)
                    (a11*b14 + a12*b24 + a13*b34 + a14*b44)
            r2 = Vec4 (a21*b11 + a22*b21 + a23*b31 + a24*b41)
                    (a21*b12 + a22*b22 + a23*b32 + a24*b42)
                    (a21*b13 + a22*b23 + a23*b33 + a24*b43)
                    (a21*b14 + a22*b24 + a23*b34 + a24*b44)
            r3 = Vec4 (a31*b11 + a32*b21 + a33*b31 + a34*b41)
                    (a31*b12 + a32*b22 + a33*b32 + a34*b42)
                    (a31*b13 + a32*b23 + a33*b33 + a34*b43)
                    (a31*b14 + a32*b24 + a33*b34 + a34*b44)
            r4 = Vec4 (a41*b11 + a42*b21 + a43*b31 + a44*b41)
                    (a41*b12 + a42*b22 + a43*b32 + a44*b42)
                    (a41*b13 + a42*b23 + a43*b33 + a44*b43)
                    (a41*b14 + a42*b24 + a43*b34 + a44*b44)
        in Mat4 r1 r2 r3 r4

instance Monoid Mat4 where
    mempty :: Mat4
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

-- | Invert a 4x4 matrix. Returns identity if singular.
inverseMat4 :: Mat4 -> Mat4
inverseMat4 (Mat4 (Vec4 a00 a01 a02 a03)
                  (Vec4 a10 a11 a12 a13)
                  (Vec4 a20 a21 a22 a23)
                  (Vec4 a30 a31 a32 a33)) =
    let
        det2x2 x y z w = x*w - y*z
        det3x3 b00 b01 b02
               b10 b11 b12
               b20 b21 b22 =
            b00*det2x2 b11 b12 b21 b22 - b01*det2x2 b10 b12 b20 b22 + b02*det2x2 b10 b11 b20 b21

        -- cofactor entries
        c00 =  det3x3 a11 a12 a13 a21 a22 a23 a31 a32 a33
        c01 = -det3x3 a10 a12 a13 a20 a22 a23 a30 a32 a33
        c02 =  det3x3 a10 a11 a13 a20 a21 a23 a30 a31 a33
        c03 = -det3x3 a10 a11 a12 a20 a21 a22 a30 a31 a32

        c10 = -det3x3 a01 a02 a03 a21 a22 a23 a31 a32 a33
        c11 =  det3x3 a00 a02 a03 a20 a22 a23 a30 a32 a33
        c12 = -det3x3 a00 a01 a03 a20 a21 a23 a30 a31 a33
        c13 =  det3x3 a00 a01 a02 a20 a21 a22 a30 a31 a32

        c20 =  det3x3 a01 a02 a03 a11 a12 a13 a31 a32 a33
        c21 = -det3x3 a00 a02 a03 a10 a12 a13 a30 a32 a33
        c22 =  det3x3 a00 a01 a03 a10 a11 a13 a30 a31 a33
        c23 = -det3x3 a00 a01 a02 a10 a11 a12 a30 a31 a32

        c30 = -det3x3 a01 a02 a03 a11 a12 a13 a21 a22 a23
        c31 =  det3x3 a00 a02 a03 a10 a12 a13 a20 a22 a23
        c32 = -det3x3 a00 a01 a03 a10 a11 a13 a20 a21 a23
        c33 =  det3x3 a00 a01 a02 a10 a11 a12 a20 a21 a22

        det = a00*c00 + a01*c01 + a02*c02 + a03*c03
        safeDet = if abs det < 1e-12 then 1 else det  -- fallback to 1 to avoid division by 0
    in Mat4
        (Vec4 (c00/safeDet) (c10/safeDet) (c20/safeDet) (c30/safeDet))
        (Vec4 (c01/safeDet) (c11/safeDet) (c21/safeDet) (c31/safeDet))
        (Vec4 (c02/safeDet) (c12/safeDet) (c22/safeDet) (c32/safeDet))
        (Vec4 (c03/safeDet) (c13/safeDet) (c23/safeDet) (c33/safeDet))
