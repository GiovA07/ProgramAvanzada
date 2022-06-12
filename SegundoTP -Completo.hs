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



insertaralfinal :: a -> [a] -> [a]
insertaralfinal e [] = e:[]
insertaralfinal e (x:[]) = x:(e:[])
insertaralfinal e (x:xs) = x: insertaralfinal e xs

-- Ejercicio 4
abs1 :: Int -> Int
abs1 a | a < 0 = (a*(-1))
       | otherwise = a

--Ejercicio 5
-- que sea primo significa que la cantidad de divisiores entre [1..n] tiene que ser 2.

esPrimo :: Int -> Bool
esPrimo n = divisores n == 2

divisores :: Int -> Int
divisores n = divisores' n n

--precondicion: n >= 1 && k <= n
divisores' :: Int -> Int -> Int
divisores' n 1 = 1
divisores' n k 
               | (n `mod` k) == 0 = 1 + divisores' n (k-1)
               | otherwise = divisores' n (k-1)


-- ES PRIMO Creado Por Mi
esPrimo' :: Int -> Bool 
esPrimo' n | n < 0 = error "Solo se puede realizar con numeros NATURALES"
esPrimo' n = [x | x <- [1..n], n `mod` x == 0] == [1,n]



--Ejercicio 6
listaPrimos :: Int -> [Int]
listaPrimos n 
              | esPrimo' n = [x | x <- [1..n], n `mod` x == 0]
              | otherwise = []


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


-- Ejercicio 9
palindromo :: (Eq a) => [a] -> Bool
palindromo xs = listIgual (reversa xs) xs


