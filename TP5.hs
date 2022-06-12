
--TP 5

--ejercicio 1
listaDe1Inf = 1: listaDe1Inf 

--ejercicio 2
listaInf :: Int -> [Int]
listaInf n = [n] ++ listaInf (succ n)

--hecho con listas por comprension
{-
listaInf' :: Int -> [Int]
listaInf' n = [x| x <- [n..]]
-}

--ejercicio 3 Generar una lista con los primeros n naturales.
listaPrimerosN :: Int -> [Int]
listaPrimerosN n 
                | n == 0 = []
                |otherwise = listaPrimerosN (n-1) ++ [n]

--hecho con listas por comprension
listaPrimerosN' :: Int -> [Int]
listaPrimerosN' n = [x | x <- [1..n]]


--ejercicio 4 Retornar los primeros 5 elementos de una lista infinita de enteros positivos.
retornar :: [Int] -> [Int]
retornar xs = take 5 xs 



--Utilizando funciones de alto orden resolver:

-- Ejercicio 5 Dada una lista de enteros, retornar sus cuadrados

cuadrado:: Int -> Int
cuadrado n = n^2

listaCuadrado' :: (Int -> Int) -> [Int] -> [Int]
listaCuadrado' _ [] = []
listaCuadrado' f (x:xs) = f x : listaCuadrado' f xs

{-Con MAP-}
listaCuadrado'' :: [Integer] -> [Integer]
listaCuadrado'' xs = map (^2) xs

  
-- Ejercicio 6. Dado un entero positivo, retornar sus divisores (CORREGIR!!!!)
divisores :: Int -> [Int]
divisores n = filter (\x -> n `mod` x == 0) 




--Ejercicio 7. Dada una lista de naturales, obtener la lista que contenga solo los numeros primos de la lista original.
listaPrimos :: (Int -> Bool) -> [Int] -> [Int]
listaPrimos f [] = []
listaPrimos f (x:xs) | f x == True = x : listaPrimos f xs 
                     | otherwise = listaPrimos f xs

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = [x|x <- [1..n], n `mod` x == 0] == [1,n]

{- Con funcion FILTER -}
listaPrimos' :: [Int] -> [Int]
listaPrimos' xs = filter (esPrimo) xs


--Ejercicio 8. Dada una lista de naturales, retornar la suma de los cuadrados de la lista.

sumaCuadrados :: (Int -> Int) -> [Int] -> [Int]
sumaCuadrados f [] = []
sumaCuadrados f (x:xs) = f x : sumaCuadrados f xs 

{- Otra forma con MAP -}

sumCuadrados :: [Integer] -> Integer
sumCuadrados xs = sum (map (^2) xs)




--Ejercicio 9. Dada una lista de naturales, retornar la lista con sus sucesores.

        --Aca le damos la funcion "SUCC" para que funcione.

listaSucesores :: (Integer -> Integer) -> [Integer] ->  [Integer]
listaSucesores _ [] = []
listaSucesores f (x:xs) = f x : listaSucesores f xs

{-Hecho con FUNCION MAP-}
listaSucesores' :: [Int] -> [Int]
listaSucesores' xs = map (succ) xs


--Ejercicio 10. Dada una lista de enteros, sumar todos sus elementos.
sumaLista :: [Int] -> Int
sumaLista xs = foldl (+) 0 xs


--Ejercicio 11. Definir el factorial usando fold.
factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]

factorial' :: Integer -> Integer
factorial' n = foldl (*) 1 [1..n]




{- Utilizando listas por comprension resolver: -}

--Ejercicio 12. Dada una lista de enteros, retornar sus sucesores
listaSucesor :: [Int] -> [Int]
listaSucesor [] = []
listaSucesor xs = [x+1 | x <- xs ]


-- Ejercicio 13. Dada una lista de naturales, retornar sus cuadrados.
listaCuadrados::[Int] -> [Int]
listaCuadrados [] = []
listaCuadrados xs = [x*2 | x <- xs]


--Ejercicio 14 Dada una lista de enteros, retornar los elementos pares que sean mayores a 10
listaMayores10 :: [Int] -> [Int]
listaMayores10 [] = []
listaMayores10 xs = [x | x <- xs, x > 10]


--Ejercicio 15. Dado un entero, retornar sus divisores.
divisoresDeN :: Int -> [Int]
divisoresDeN n = [x | x <- [1..n], n `mod` x == 0]


--Ejercicio 16 Dado un natural n, retornar los numeros primos comprendidos entre 2 y n.
primos :: Int -> [Int]
primos n = [x | x <- [2..n], divi x == [1,x]]
         where divi = divisoresDeN


-- Ejercicio 17. Dadas dos listas de naturales, retornar su producto cartesiano.
productoCartesiano :: [Int] -> [Int] -> [(Int, Int)]
productoCartesiano xs ys = [(x,y)| x <- xs, y <- ys]



--Ejercicio 18 . Definir la lista infinita de los numeros pares.

listaParInf = [x | x <- [1..], x `mod` 2 == 0]

listaParInf' = [x | x <- [1..],  even x]











--Listas por Comprension
paresDel2AlN :: Int -> [Int]
paresDel2AlN n | n < 2 = []
               |otherwise = paresDelAalB 2 n
               

paresDelAalB :: Int -> Int -> [Int]
paresDelAalB a b | a <= b && (esPar a) = a : paresDelAalB (a+1) b
                 |a <= b = paresDelAalB (a+1) b
                 |otherwise = []


        
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0
--paresDel2AlN 10
--paresDel2AlN 2 10
--2: (paresDElAalB 3 10)
--2: (4 : (paresDelAalB))



paresDel2AlN':: Int -> [Int]
paresDel2AlN' n = [x | x <- [1..n], mod x 2 == 0]



{-LISTAS POR COMPRENCION EJEMPLOS!!-}
-- [x | x <- [1..n], mod x 2 == 0]                                                 //lista de pares
--[(x,y)| x<- [1..10], y <- [1..10], x `mod` 2 == 0 && y `mod` 2 /= 0]             //lista de tuplas de x pares e y impares






{- ALTO ORDEN-}
foo :: [Int] -> (Int -> Bool) -> Int
foo [] _ = 0
foo (x:xs) filtro 
                | filtro x = x + (foo xs filtro)
                | otherwise = foo xs filtro
                

--Por ejemplo si ejecuto     foo [1,2,3,4,5,6] esPar     esto devolvera 12, ya que suma todos los valores pares de la lista.

--Filter

{-foldr
foldr (&&) True [x `mod` 2 == 0 | x <- [1..10], x `mod` 2 == 0]

foldr (&&) True [x `mod` 2 == 0 | x <- [1..10]]

foldr (&&) True (map )

-}


{-
Funcion MAP
map esPar [1,2,3,4,5]

devolveria [False, True, False, True, False]

-}


--foldl


