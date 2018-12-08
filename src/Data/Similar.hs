module Data.Similar (Similar(), fromString, getString) where


newtype Similar = Similar { getString :: String } deriving (Eq, Show)


fromString :: String -> Similar
fromString = Similar


instance Ord Similar where
  compare sim1 sim2 =
    compareSimilar False (getString sim1) (getString sim2)


compareSimilar :: Bool -> String -> String -> Ordering
compareSimilar False (l1:s1) (l2:s2) = compareSimilar (l1 /= l2) s1 s2
compareSimilar False _ _ = LT
compareSimilar True s1 s2 = compare s1 s2
