{-TRABAJO PRACTICO NUMERO 10 DE PROGRAMACION AVANZADA  INDUCCION Y DERIVACION FUNCIONALES-}




--Ejercicio 4--
crec :: (Ord a) => [a] -> Bool
crec [] = True
crec [x] = True
crec (x:xs) = x <= xs!!0 && (crec xs)



--Ejercicio 5--
minimoList :: [Int] -> Int
minimoList [] = error "sos boludo?, no existe min en lista vacia"
minimoList [x] = x
minimoList (x:xs) = min x (minimoList xs)


--Ejercicio 6--
igualresto :: [Int] -> Bool
igualresto xs = igualresto' xs 0

igualresto' :: [Int] -> Int -> Bool
igualresto' [] _ = False
igualresto' (x:xs) k = x == k + sum xs || igualresto' xs (x+k)