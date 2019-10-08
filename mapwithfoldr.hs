map :: (a -> b) -> [a] -> [b]
map func xs = foldr (\elem acc -> func elem:acc) [] xs
