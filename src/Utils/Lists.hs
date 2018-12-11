module Utils.Lists (foldPairsL, foldPairsR, keep, safeHead) where


foldPairsL :: (a -> b -> b -> a) -> a -> [b] -> a
foldPairsL f a =
  snd . foldl (pairUpL (\b1 b2 a -> f a b1 b2)) ( Nothing, a )


foldPairsR :: (b -> b -> a -> a) -> a -> [b] -> a
foldPairsR f a =
  snd . foldr (pairUpR f) ( Nothing, a )


keep :: (a -> Maybe b) -> [a] -> [b]
keep f items =
  foldr (folder f) [] items


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (head:_) = Just head


pairUpL :: (b -> b -> a -> a) -> ( Maybe b, a ) -> b -> ( Maybe b, a )
pairUpL f ( Just b', a ) b = ( Just b, f b' b a )
pairUpL f ( Nothing, a ) b = ( Just b, a )

pairUpR :: (b -> b -> a -> a) -> b -> ( Maybe b, a ) -> ( Maybe b, a )
pairUpR f b ( Just b', a ) = ( Just b, f b' b a )
pairUpR f b ( Nothing, a ) = ( Just b, a )


folder :: (a -> Maybe b) -> a -> [b] -> [b]
folder f a items =
  case f a of
    Nothing ->
      items
    Just b ->
      b:items
