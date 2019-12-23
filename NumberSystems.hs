import Data.Char
import Data.List

toDecimal:: Int->String->String
toDecimal 1 snumber = show $ fromTuring snumber (-1) where
            fromTuring "" acc = acc
            fromTuring (s:snum) acc = if s == '1' then fromTuring snum (acc+1) else error "invalid digit detected"
toDecimal base snumber | (base > 62) || (base < 1) = error "Uncorrect base"
                       | otherwise = show $ helper  snumber 0 0
      where alphabet = ['0'..'9']++['a'..'z']++['A'..'Z']
            fromMaybe (Just a) = a
            helper (s:snum) acc count | (elem s alphabet) = foldl (\acc x -> acc * base + (fromMaybe(elemIndex x alphabet))) (fromMaybe(elemIndex s alphabet)) snum
                                      | otherwise = error "invalid digit detected"

fromDecimal::Int->String->String
fromDecimal 1 snum = replicate ((read snum) + 1) '1'
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
                                                                               else if x<10 then (chr(x+48)):(trans xs)
                                                                                 else error "Incorrect Base"
convertFromTo::Int->Int->String->String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ toDecimal fromBase snumber 

																						
