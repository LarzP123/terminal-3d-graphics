module Objects where
import Tri ( Tri(..) )
import Vector ( Vec3(Vec3) )

-- | A cube
cube :: [Tri Vec3]
cube =
    [
        -- FRONT (z = 10) – white
        Tri (Vec3 (-10) (-10)  10) (Vec3 10 (-10)  10) (Vec3 10 10  10) 'w',
        Tri (Vec3 (-10) (-10)  10) (Vec3 10 10  10) (Vec3 (-10) 10  10) 'w',

        -- BACK (z = -10) – yellow
        Tri (Vec3 10 (-10) (-10)) (Vec3 (-10) (-10) (-10)) (Vec3 (-10) 10 (-10)) 'y',
        Tri (Vec3 10 (-10) (-10)) (Vec3 (-10) 10 (-10)) (Vec3 10 10 (-10)) 'y',

        -- LEFT (x = -10) – red
        Tri (Vec3 (-10) (-10) (-10)) (Vec3 (-10) (-10) 10) (Vec3 (-10) 10 10) 'r',
        Tri (Vec3 (-10) (-10) (-10)) (Vec3 (-10) 10 10) (Vec3 (-10) 10 (-10)) 'r',

        -- RIGHT (x = 10) – green
        Tri (Vec3 10 (-10) 10) (Vec3 10 (-10) (-10)) (Vec3 10 10 (-10)) 'g',
        Tri (Vec3 10 (-10) 10) (Vec3 10 10 (-10)) (Vec3 10 10 10) 'g',

        -- TOP (y = 10) – blue
        Tri (Vec3 (-10) 10 10) (Vec3 10 10 10) (Vec3 10 10 (-10)) 'b',
        Tri (Vec3 (-10) 10 10) (Vec3 10 10 (-10)) (Vec3 (-10) 10 (-10)) 'b',

        -- BOTTOM (y = -10) – cyan
        Tri (Vec3 (-10) (-10) (-10)) (Vec3 10 (-10) (-10)) (Vec3 10 (-10) 10) 'c',
        Tri (Vec3 (-10) (-10) (-10)) (Vec3 10 (-10) 10) (Vec3 (-10) (-10) 10) 'c'
    ]

tree :: [Tri Vec3]
tree =
    [
        ------------------------------------------------------------------
        -- TRUNK  (rectangular prism from y = -10 to y = 0)
        ------------------------------------------------------------------
        -- Dimensions:
        --   x: [-2, 2]
        --   z: [-2, 2]
        --   y: [-10, 0]
        -- Color: 'b' (b)

        -- Front face (z = 2)
        Tri (Vec3 (-2) (-10) 2) (Vec3 2 (-10) 2) (Vec3 2 0 2) 'b',
        Tri (Vec3 (-2) (-10) 2) (Vec3 2 0 2) (Vec3 (-2) 0 2) 'b',

        -- Back face (z = -2)
        Tri (Vec3 2 (-10) (-2)) (Vec3 (-2) (-10) (-2)) (Vec3 (-2) 0 (-2)) 'b',
        Tri (Vec3 2 (-10) (-2)) (Vec3 (-2) 0 (-2)) (Vec3 2 0 (-2)) 'b',

        -- Left face (x = -2)
        Tri (Vec3 (-2) (-10) (-2)) (Vec3 (-2) (-10) 2) (Vec3 (-2) 0 2) 'b',
        Tri (Vec3 (-2) (-10) (-2)) (Vec3 (-2) 0 2) (Vec3 (-2) 0 (-2)) 'b',

        -- Right face (x = 2)
        Tri (Vec3 2 (-10) 2) (Vec3 2 (-10) (-2)) (Vec3 2 0 (-2)) 'b',
        Tri (Vec3 2 (-10) 2) (Vec3 2 0 (-2)) (Vec3 2 0 2) 'b',

        -- Top of trunk (y = 0)
        Tri (Vec3 (-2) 0 (-2)) (Vec3 2 0 (-2)) (Vec3 2 0 2) 'b',
        Tri (Vec3 (-2) 0 (-2)) (Vec3 2 0 2) (Vec3 (-2) 0 2) 'b',

        ------------------------------------------------------------------
        -- LEAVES  (pyramid from y=0 to y=12)
        ------------------------------------------------------------------
        -- Base: square [-8,8] x [-8,8] at y=0
        -- Apex: (0, 12, 0)
        -- Color: 'g' (green)

        -- Front triangle
        Tri (Vec3 (-8) 0  8) (Vec3 8 0  8) (Vec3 0 12 0) 'g',

        -- Right triangle
        Tri (Vec3 8 0  8) (Vec3 8 0 (-8)) (Vec3 0 12 0) 'g',

        -- Back triangle
        Tri (Vec3 8 0 (-8)) (Vec3 (-8) 0 (-8)) (Vec3 0 12 0) 'g',

        -- Left triangle
        Tri (Vec3 (-8) 0 (-8)) (Vec3 (-8) 0  8) (Vec3 0 12 0) 'g'
    ]

