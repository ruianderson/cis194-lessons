data IntList = Empty | Cons Int IntList
  deriving Show

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList f Empty = Empty
filterIntList f (Cons x xs)
  | f x       = Cons x (filterIntList f xs)
  | otherwise = filterIntList f xs

data List t = E | C t (List t)

filterList :: (Int -> Bool) -> IntList -> IntList
filterList _ E = E
filterList f (C x xs)
  | f x       = C x (filterList f xs)
  | otherwise = filterList f xs
