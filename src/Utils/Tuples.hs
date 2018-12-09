module Utils.Tuples (map1of2, map2of2) where


map1of2 :: (a -> c) -> ( a, b ) -> ( c, b )
map1of2 f ( a, b ) = ( f a, b )


map2of2 :: (b -> c) -> ( a, b ) -> ( a, c )
map2of2 f ( a, b ) = ( a, f b )
