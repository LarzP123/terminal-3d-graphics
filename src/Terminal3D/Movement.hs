module Terminal3D.Movement where
import Terminal3D.Vector
import Data.List (find)

{-| A possible movement operation containning a position transform (rotation -> position -> output position)
    , rotation transform, action character, and full name -}
data MoveOperation = MoveOperation (Vec3 -> Vec3 -> Vec3) (Vec3 -> Vec3) Char String

-- | A list of operations and how they transform the 3d spacial and rotational coordinates
moveOperations :: [MoveOperation]
moveOperations =
    [
        MoveOperation (const id) id 'n' "do nothing",
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x - speed * sin yaw) y (z + speed * cos yaw)) id 'w' "move forward",
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x + speed * sin yaw) y (z - speed * cos yaw)) id 's' "move backward",
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x - speed * cos yaw) y (z - speed * sin yaw)) id 'd' "strafe right",
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x + speed * cos yaw) y (z + speed * sin yaw)) id 'a' "strafe left",
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 p (y - yawInc) r)   'j' "turn left",
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 p (y + yawInc) r)   'l' "turn right",
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 (p + pitchInc) y r) 'i' "turn up",
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 (p - pitchInc) y r) 'k' "turn down"
    ]
  where
    speed    = 5
    pitchInc = 0.2
    yawInc   = 0.2

-- | Optional boundaries of a world the player cannot leave past as (minX, maxX, minZ, maxZ).
type Bounds = (Int, Int, Int, Int)

-- | Push a position back inside the bounds. Makes it so the player can't leave the area
clampToBounds :: Maybe Bounds -> Vec3 -> Vec3
clampToBounds Nothing pos = pos
clampToBounds (Just (minX, maxX, minZ, maxZ)) (Vec3 x y z) =
    Vec3 (clamp minX maxX x) y (clamp minZ maxZ z)
  where
    clamp lo hi = max (fromIntegral lo) . min (fromIntegral hi)

-- | Parse a movement command and return updated (position, rotation), or Nothing if invalid.
move :: Maybe Bounds -> String -> Vec3 -> Vec3 -> Maybe (Vec3, Vec3)
move bounds cmd pos rot =
    case find (\(MoveOperation _ _ c name) -> name == cmd || [c] == cmd) moveOperations of
        Just (MoveOperation posT rotT _ _) -> Just (clampToBounds bounds (posT rot pos), rotT rot)
        Nothing                            -> Nothing

