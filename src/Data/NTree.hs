module Data.NTree
  (NTree(), empty, singleton, children, insert, parent, values)
  where


data NTree a = Empty | NTree a [NTree a] deriving (Eq, Show)


empty :: NTree a
empty = Empty


singleton :: a -> NTree a
singleton value =
  NTree value []


children :: NTree a -> [NTree a]
children Empty = []
children (NTree _ children) = children


insert :: a -> NTree a -> NTree a
insert value Empty = singleton value
insert value (NTree a children) = NTree a ((NTree value []):children)


values :: NTree a -> [a]
values Empty = []
values (NTree a children) = a:(concat (fmap values children))


null :: NTree a -> Bool
null Empty = True
null _ = False


parent :: [NTree a] -> a -> NTree a
parent children value = NTree value children


instance Functor NTree where
  fmap f Empty = Empty
  fmap f (NTree val children) = NTree (f val) (map (fmap f) children)

instance Foldable NTree where
  foldr f dflt Empty = dflt
  foldr f dflt (NTree val []) = f val dflt
  foldr f dflt (NTree val (child:more)) = foldr f (foldr f dflt child) (NTree val more)
