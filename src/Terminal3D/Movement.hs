module Terminal3D.Movement where
import Terminal3D.Vector
import Data.List (find)
import Terminal3D.Localization

{-| A possible movement operation containning a position transform (rotation -> position -> output position)
    , rotation transform, action character, and full name -}
data MoveOperation = MoveOperation (Vec3 -> Vec3 -> Vec3) (Vec3 -> Vec3) Char Local

-- | A list of operations and how they transform the 3d spacial and rotational coordinates
moveOperations :: [MoveOperation]
moveOperations =
    [
        MoveOperation (const id) id 'n' commandDoNothing,
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x - speed * sin yaw) y (z + speed * cos yaw)) id 'w' commandForward,
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x + speed * sin yaw) y (z - speed * cos yaw)) id 's' commandBackward,
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x - speed * cos yaw) y (z - speed * sin yaw)) id 'd' commandRight,
        MoveOperation (\(Vec3 _ yaw _) (Vec3 x y z) -> Vec3 (x + speed * cos yaw) y (z + speed * sin yaw)) id 'a' commandLeft,
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 p (y - yawInc) r)   'j' commandTurnLeft,
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 p (y + yawInc) r)   'l' commandTurnRight,
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 (p + pitchInc) y r) 'i' commandTurnUp,
        MoveOperation (const id) (\(Vec3 p y r) -> Vec3 (p - pitchInc) y r) 'k' commandTurnDown
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
move :: Maybe Bounds -> String -> Vec3 -> Vec3 -> (Local -> String) -> Maybe (Vec3, Vec3)
move bounds cmd pos rot translator =
    case find (\(MoveOperation _ _ c name) -> translator name == cmd || [c] == cmd) moveOperations of
        Just (MoveOperation posT rotT _ _) -> Just (clampToBounds bounds (posT rot pos), rotT rot)
        Nothing                            -> Nothing

