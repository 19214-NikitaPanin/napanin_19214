map :: (a -> b) -> [a] -> [b]
map func xs = foldl (\a b -> a++[func b]) [] xs
