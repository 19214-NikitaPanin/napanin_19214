import System.Random
import System.IO
import Data.Char
import Data.List

vowels = ['a', 'e', 'i', 'o', 'u'] -- список гласных
consonants = ['a'..'z'] \\ vowels -- согласных
subcons = consonants \\ ['q', 'w', 'c', 'v', 'j'] --согласные, которые относительно соч. друг с другом

getResult:: Int -> Char -> [Int] -> [Int] -> [Int] -> String
getResult _ _ [] _ _ = []
getResult style first (rc1:randomsC1) (rc2:randomsC2) (rv:randomsV) | style == 0 && elem first vowels = case1
								    | style == 0 && not (elem first vowels) = case2
								    | style == 1 && elem first vowels = case3
								    | otherwise = case4
							             where
				case1 = (consonants !! rc1) : getResult style (consonants !! rc1) randomsC1 randomsC2 randomsV
				case2 = (vowels !! rv) : getResult style (vowels !! rv) randomsC1 randomsC2 randomsV
				case3 = (subcons !! rc2): getResult style (subcons !! rc2) randomsC1 randomsC2 randomsV
				case4 = (vowels !! rv) : (subcons !! rc2) : getResult style (vowels !! rv) randomsC1 randomsC2 randomsV


beauty1 res style |style == 1 = init res
                  |otherwise = res

beauty2 rule rule2 res = concat(map (\x -> if x == 'c' && rule == 0 then ['c', 'k']
                                             else if x == 'c' && rule < 4 then ['c', 'h']
						 else if x == 's' && rule2 == 0 then ['s', 'h']
						    else [x]) res)

beauty3 res rule | elem (last res) (consonants \\ ['y']) && rule < 7 = add res rule
                 |otherwise = res
                 where
                   add res rule = if rule < 2 then res ++ ['e']
				    else if rule < 4 then res ++ ['y']  
                                      else if rule < 7 then res ++ ['e', 'y']
                                        else res									  
main:: IO String 
main = do
 first <- randomRIO('a', 'z') -- рандомим 1-ую букву
 len <- randomRIO (4, (7::Int))
 h_Or_K_AfterC <-randomRIO(0, 9::Int) --  0 - 'k', 1,2,3 - 'h' после 'с', иначе просто оставляем эту 'c' в покое
 h_AfterS <- randomRIO(0, 5::Int) -- 0 - да
 style <- randomRIO(0, (1::Int))-- 0 - чередование (гласная ->согласная), 1 - (две согласные -> гласная(почти))
 e_y_ey_InTheEnd <-randomRIO(0, 9::Int) -- 0, 1 - 'e'. 2, 3 - 'y'. 4, 5, 6 - 'ey'. Стреляет, только если в конце согласная  
 randomsC1 <- mapM (\x ->randomRIO (0, length consonants - 1)) [1..len]
 randomsC2 <- mapM (\x ->randomRIO (0, length subcons - 1)) [1..len]
 randomsV <- mapM (\x ->randomRIO (0, length vowels - 1)) [1..len]
 let preLog = getResult style first randomsC1 randomsC2 randomsV -- строим таки
 let preRes1 = beauty1 (first:preLog) style
 let preRes2 = beauty2 h_Or_K_AfterC h_AfterS preRes1
 let res = beauty3 preRes2 e_y_ey_InTheEnd -- можно было бы объединить все beauty, но стало бы еще нечитаемее
 return res	 

getRes suspect words |isInfixOf suspect words = []
                     |otherwise = suspect

toIO:: [Char] -> IO [Char]
toIO xs = do
 return xs
 
parse::[Char] -> IO [Char]
parse xs = if xs == [] then generate else toIO xs --чтобы возвращал одно и то же 

generate = do
 words <- readFile "dictionary.txt"
 suspect <- main 
 let search = getRes suspect words 
 let outer = parse search
 res <- outer 
 return res
