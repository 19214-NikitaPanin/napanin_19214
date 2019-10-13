--1
get' :: [a] -> Int -> a
get' [] n = error "no such element in the list"
get' (x:xs) n | n < 0 = error "n < 0, there cannot be an element with negative index " 
			  | n == 0 = (x)
			  |	n > 0 = get' xs (n-1)			  

--2
head' :: [a] -> a
head' [] = error "list with no elements"
head' (x:xs) = x

--3
last' :: [a]-> a
last' [x] = x
last' (x:xs) = last xs  

--4
init'::[a]->[a]
init' [x]=[]
init' (x:xs) = x : init' xs

--5
tail' :: [a]->[a]
tail' [] = error "list with no elements"
tail' (x:xs) = xs

--6
length':: [a] -> Integer
length' xs = helper 0 xs where
						helper n []= n
						helper n (x:xs) = helper (n+1) xs

--7
append'::[a]->a->[a]
append' xs x= xs ++ [x] 		

--8
null':: [a] ->  Bool 
null' [] = True
null' xs = False

--9
reverse':: [a]->[a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] 

--10
concat' ::[a]->[a]->[a]
concat' xs [] = xs
concat' xs (y:ys) = concat' (xs ++ [y]) ys

--11
drop':: Integer -> [a] -> [a]
drop' 0 xs = xs 
drop' n (x:xs) = drop' (n-1) xs 

--12
take':: Integer -> [a] -> [a]
take' 0 xs = []
take' n (x:xs) = [x] ++ take' (n-1) xs  

--13
splitAt':: Integer -> [a] -> ([a],[a])
splitAt' n xs = (take' (n-1) xs, drop' (n-1) xs)

--14
elem':: Eq a => [a] -> a -> Bool
elem' [] s = False
elem' (x:xs) s = if s==x then True else elem' xs s

--15
filter':: (a -> Bool) -> [a] -> [a]
filter' test [] = []
filter' test (x:xs) = if test x == True then [x] ++ filter' test xs else filter' test xs 

--16
map':: (a -> b)->[a]->[b]
map' f [] = []
map' f (x:xs) = [f x] ++ map' f xs

--17
zip':: [a]->[a]->[(a,a)]
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys


	

		





