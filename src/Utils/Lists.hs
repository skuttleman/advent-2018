module Utils.Lists (foldPairsL, foldPairsR) where


foldPairsL :: (a -> b -> b -> a) -> a -> [b] -> a
foldPairsL f a =
  snd . foldl (pairUpL (\b1 b2 a -> f a b1 b2)) ( Nothing, a )


foldPairsR :: (b -> b -> a -> a) -> a -> [b] -> a
foldPairsR f a =
  snd . foldr (pairUpR f) ( Nothing, a )


pairUpL :: (b -> b -> a -> a) -> ( Maybe b, a ) -> b -> ( Maybe b, a )
pairUpL f ( Just b', a ) b = ( Just b, f b' b a )
pairUpL f ( Nothing, a ) b = ( Just b, a )

pairUpR :: (b -> b -> a -> a) -> b -> ( Maybe b, a ) -> ( Maybe b, a )
pairUpR f b ( Just b', a ) = ( Just b, f b' b a )
pairUpR f b ( Nothing, a ) = ( Just b, a )

