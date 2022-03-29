-- funcion :: (Ord a)=> [a] -> [a] -> Bool

-- nombreFuncion:: Restricciones => Perfil


{-
existe:: [int] -> Int -> Bool
existe [] e = False
existe (x:xs) e | x == e = True
		        | otherwise = existe xs e
-}

{- 
EJEMPLo de como funciona:
existe [1,3] 3
existe (1:[3]) 3
existe [3] 3
existe (3: []) 3
True
 -}

--Segundo Ejercicio
hd::[a] -> a
hd (x:xs) = x

tl::[a] -> [a]
tl (x:xs) = xs

last1::[a] -> a
last1 [x] = x
last1 (x:xs) = last1 xs

-- otra forma de hacerlo
{-
last1::[a] -> a
last1 (x:xs) = head(reverse xs)
-}


init1 :: [a] -> [a]
init1 [x] = []
init1 (x:xs) = x: init1 xs

--Tercer Ejercicio
concatenar :: [a] -> [a] -> [a]
concatenar [] (y:ys) = (y:ys)
concatenar (x:xs) [] = (x:xs)
concatenar (x:xs) (y:ys) = x: concatenar xs (y:ys)
             
             
             
tomar :: Int -> [a] -> [a]
tomar n (x:xs) | n == 0 = []
               | n == 1 = [x]
               | otherwise = x: tomar (n-1) xs  



tirar :: Int -> [a] -> [a]
tirar n (x:xs)| n == 0 = (x:xs)
               | null xs = []
               | otherwise = tirar (n-1) xs


--Ejercicio 3

--insertaralfinal :: a -> [a] -> [a]
--insertaralfinal elem (x:xs)| (x:xs) == ([]:[]) = elem

-- Ejercicio 4
abs1 :: Int -> Int
abs1 a | a < 0 = (a*(-1))
       | otherwise = a

--Ejercicio 5

--Ejercicio 6


-- Ejercicio 7
reversa:: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--Ejercicio 8
listIgual :: Eq a => [a] -> [a] -> Bool
listIgual [] [] = True
listIgual [] xs = False
listIgual xs [] = False
listIgual (x:xs) (y:ys) | x == y = listIgual xs ys
                        | x /= y = False


--Ejercicio 9
palindromo :: Eq a => [a] -> Bool
palindromo xs | listIgual (reversa(xs) xs) == True = True