data Complex num = ToComplex num num

instance (Num num, Ord num, Eq num, Show num) => Show (Complex num) where 
											show (ToComplex re 0) = show re
													
											show (ToComplex 0 im)  |im < 0 = if im == -1 then show "-i" else show im ++ "i"
																   |im > 0 = if im == 1 then show "i" else show im ++ "i"
																		   
											show (ToComplex re im) |im < 0 = if im == -1 then show re ++ "-i" else show re ++ show im ++ "i"
																   |im > 0 = if im == 1 then show re ++ "i" else show re ++ show im ++ "i"
																			 
instance (Eq num) =>  Eq (Complex num) where
											(==) (ToComplex re im) (ToComplex re1 im1) | (re == re1)&&(im == im1) = True
																					   | otherwise = False
						
instance (Num num, Ord num) =>  Ord (Complex num) where
											compare (ToComplex re im) (ToComplex re1 im1) = compare (re * re + im * im) (re1 * re1 + im1 * im1)
													
data QuantumState num = ToQuantumState num String

instance (Show num) => Show (QuantumState num) where
											show(ToQuantumState complex state) = show complex ++ " State: " ++ state
											
instance (Ord num, Show num) => Ord (QuantumState num) where
											compare (ToQuantumState complex state) (ToQuantumState complex1 state1) = compare complex complex1
											
instance (Eq num) => Eq (QuantumState num) where
											(==) (ToQuantumState complex state) (ToQuantumState complex1 state1) = if (complex == complex1)&&(state == state1) then True
																														else False

instance Functor QuantumState where
						fmap f (ToQuantumState complex state) = ToQuantumState (f complex) state
	
type Qubit a = [QuantumState a]

toList :: Qubit (Complex a) -> [Complex a]
toList [] = []
toList ((ToQuantumState complex state):xs) = complex : (toList xs)

toLabelList:: Qubit a->[String]
toLabelList [] = []
toLabelList ((ToQuantumState complex state):xs) = state : (toLabelList xs)

fromList :: [Complex a] ->[String] -> Qubit (Complex a)
fromList [] [] = []
fromList (complex:xs) (state:xz) = (ToQuantumState complex state) : fromList xs xz

toPairList:: Qubit (Complex a) -> [(Complex a,String)]
toPairList [] = []
toPairList ((ToQuantumState complex state):xs) = (complex,state):(toPairList xs)

fromPairList:: [(Complex a,String)] -> Qubit (Complex a)
fromPairList [] = []
fromPairList ((complex,state):xs) = (ToQuantumState complex state): (fromPairList xs)

--scalarProduct :: (Num a) => Qubit (Complex a) -> Qubit (Complex a) -> Complex a
--scalarProduct kub kub1 = lenA kub * lenB kub1 * cosinus kub kub1 where 
												--lenA (ToQuantumState (ToComplex re im) state) = sqrt (re*re + im * im)
												--lenB (ToQuantumState (ToComplex re im) state) = sqrt (re*re + im * im)
												--cosinus (ToQuantumState (ToComplex re im) state) (ToQuantumState (ToComplex re1 im1) state1) = (re*re + im * im) / (lenA * lenB)

--entagle:: (Num a) => Qubit a -> Qubit a -> Qubit a
--entagle (ToQuantumState (ToComplex re im) state) (ToQuantumState (ToComplex re1 im1) state1) = (ToQuantumState (ToComplex (re * re1) (im * im1)) (state ++ state1))








	
	
	
	
