module Matrix where
import Vector ( Vec4(..), Dot(dot) )

-- | A 4x4 Matrix.
data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4 deriving (Show, Eq)

-- | Does a 4x4 Matrix times a column Vector of 4 components
multMatVec :: Mat4 -> Vec4 -> Vec4
multMatVec (Mat4 r1 r2 r3 r4) colV = Vec4 (dot r1 colV) (dot r2 colV) (dot r3 colV) (dot r4 colV)

-- https://www.mauriciopoppe.com/notes/computer-graphics/viewing/projection-transform/ Eq. 12
symmetricPerspectiveMatrix :: Double -> Double -> Double -> Double -> Mat4
symmetricPerspectiveMatrix r n t f = Mat4
    (Vec4 (n/r) 0     0             0            )
    (Vec4 0     (n/t) 0             0            )
    (Vec4 0     0     ((f+n)/(n-f)) (2*f*n/(n-f)))
    (Vec4 0     0     (-1)            0          )
