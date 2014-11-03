filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p l = [x | x <- l, p x]


map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f l = [f x | x <- l]


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v []     = v
foldr' f v (x:xs) = f x $ foldr' f v xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v []     = v
foldl' f v (x:xs) = foldl' f (f v x) xs
