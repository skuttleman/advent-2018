module Data.Mover (Mover(), adjust, move, mover, position, velocity) where


data Mover a = Mover a a deriving (Eq, Ord, Show)


adjust :: (Num a) => (a -> a) -> Mover a -> Mover a
adjust f (Mover pos velocity) = Mover pos $ f velocity


move :: (Num a) => a -> Mover a -> Mover a
move amount (Mover pos velocity) = Mover (pos + amount * velocity) velocity


mover :: (Num a) => a -> a -> Mover a
mover = Mover


position :: Mover a -> a
position (Mover pos _) = pos


velocity :: Mover a -> a
velocity (Mover _ velocity) = velocity
