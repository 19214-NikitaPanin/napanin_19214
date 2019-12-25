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

instance (Num a, Show a, Ord a, Eq a) => Num (Complex a) where
										(+) (ToComplex re1 im1) (ToComplex re2 im2) = ToComplex (re1 + re2) (im1 + im2)
										(*) (ToComplex re1 im1) (ToComplex re2 im2) = ToComplex (re1*re2 - im1 * im2) (re1*re2 + im1 * im2)
										abs (ToComplex r i) = ToComplex (abs r) (abs i)
										negate (ToComplex re im) = ToComplex re (negate im)
										fromInteger int  = ToComplex (fromInteger int) 0  
										signum (ToComplex re im) = ToComplex (signum re) 0

	
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

scalarProduct:: (Num a, Show a, Ord a, Eq a) => Qubit (Complex a) -> Qubit (Complex a) -> a
scalarProduct qubit1 qubit2 =  foldl (+) 0 $ zipWith (\ (ToQuantumState (ToComplex r1 i1) _ ) (ToQuantumState (ToComplex r2 i2) _ ) -> r1*r2 + i1*i2) qubit1 qubit2

entagle::(Num a) => Qubit a ->Qubit a ->Qubit a
entagle qubit1 qubit2 = [ToQuantumState  (complex1*complex2) (state1++state2) |  (ToQuantumState complex1 state1) <- qubit1, (ToQuantumState complex2 state2) <- qubit2]
