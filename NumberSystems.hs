import Data.Char

toDecimal:: Int->String->String
toDecimal base [] = []
toDecimal base snumber = if base >=1 && base <=61 then show (foldl (+) 0 (array snumber)) else error"Incorrect Base"  
																where 
																	helper numb | 48 <= ord numb && ord numb <= 57 = ord numb-48
																				| 97 <= ord numb && ord numb<= 122 = ord numb-87
																				| 65 <= ord numb && ord numb<= 90 = ord numb-38
																				| otherwise = error "Incorrect Input"
																	array [] = []
																	array (x:snumber) = (helper(x)*(base^(length snumber))) : (array snumber)				
fromDecimal::Int->String->String
fromDecimal toBase snumber = trans $ reverse $ convert $ inter $ number snumber
              																            where 
							                      									              helper numb | 48 <= ord numb && ord numb <= 57 = ord numb-48
																		                          	                | 97 <= ord numb && ord numb<= 122 = ord numb-87
																			                                          | 65 <= ord numb && ord numb<= 90 = ord numb-38
																			                                          | otherwise = error "Incorrect Input"
                                                                  	number []=[]
																                                    number (x:snumber) = (helper(x)) : (number snumber)
													                                    			inter [] = 0
																                                    inter  (x:xs) = x*(10^(length xs)) + inter xs   
																                                    convert a | a < toBase = a:[]
                                                                              | otherwise =  (mod a toBase) : (convert (div a toBase))  
																                                    trans []=[]
																                                    trans (x:xs) = if x>9 && x<37 then (chr(x+87)):(trans xs) 
																				                                          else 
																					                                            if x>36 && x<62 then (chr(x+28)):(trans xs) 
																						                                               else 
                                                                                              if x<10 then (chr(x+48)):(trans xs)
																							                                                    else error "Incorrect Base"

convertFromTo::Int->Int->String->String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ toDecimal fromBase snumber 

																						
