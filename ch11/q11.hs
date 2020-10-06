filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) =
  if p x
    then x : filter' p xs
    else filter' p xs

map' :: (t -> a) -> [t] -> [a]
map' f [] = []
map' f (x:xs) = f x : map' f xs

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

foldr' :: (t -> a -> t) -> t -> [a] -> t
foldr' f init [] = init
foldr' f init (x:xs) = foldr' f newInit xs
  where newInit = f init x