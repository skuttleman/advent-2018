module Data.Vector2D (Vector2D(), minVector2D, maxVector2D, vector2D, vector2DToTuple) where


data Vector2D a = Vector2D a a deriving (Eq, Ord, Show)


vector2D :: (Num a) => a -> a -> Vector2D a
vector2D = Vector2D


vector2DToTuple :: Vector2D a -> ( a, a )
vector2DToTuple (Vector2D a b) = ( a, b )


instance (Num a) => Num (Vector2D a) where
  (Vector2D x1 y1) + (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
  (Vector2D x1 y1) * (Vector2D x2 y2) = Vector2D (x1 * x2) (y1 * y2)
  abs (Vector2D x y) = Vector2D (abs x) (abs y)
  signum (Vector2D x y) = Vector2D (signum x) (signum y)
  fromInteger i = Vector2D (fromInteger i) (fromInteger i)
  negate (Vector2D x y) = Vector2D (negate x) (negate y)


minVector2D :: (Ord a) => Vector2D a -> Vector2D a -> Vector2D a
minVector2D = combine min


maxVector2D :: (Ord a) => Vector2D a -> Vector2D a -> Vector2D a
maxVector2D = combine max


combine :: (a -> a -> a) -> Vector2D a -> Vector2D a -> Vector2D a
combine f (Vector2D x1 y1) (Vector2D x2 y2) =
  Vector2D (f x1 x2) (f y1 y2)
