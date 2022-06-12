-- Clase 3
{-
Intercala los elementos de dos listas en una
ejemplo: intercalar [1,3] [2,4] = [1,2,3,4]
-}
intercalar :: [a] -> [a] -> [a]
intercalar xs [] = xs
intercalar [] ys = ys
intercalar (x:xs) (y:ys) = [x,y] ++ (intercalar xs ys)

{-
Separa los elementos de una lista en una tupla.
El primer elemento de la tupla contiene todos los valores de la
lista original en posiciones pares
El segundo elemento de la tupla contiene todos los valores de la lista original en posiciones impares
ejemplo: separar [1,2,3] = ([1,3], [2])
-}
separar :: [a] -> ([a],[a])
separar [] = ([],[])
separar [x] = ([x],[])
separar (x:(y:ys)) = (x:restoI, y:restoD)
                 where (restoI, restoD) = separar ys

{-
Dada una lista ordenada en orden ascendente y un valor, utiliza
una búsqueda dicotómica para buscar el valor.
Retornar True si y solo si la lista contiene al valor
ejemplo: existe [1,2,3,4] 2 = True
-}
existe :: [Int] -> Int -> Bool
existe [] e = False
existe [x] e = x == e
existe xs e | e == primeroDer = True
            | e > primeroDer = existe parteDer e
            | otherwise = existe parteIzq e
     where longitud = length xs
           mitad = (longitud `div` 2) --div es la división entera
           parteIzq = take mitad xs
           parteDer = drop mitad xs
           primeroDer = (head parteDer)
{-
Determina si un valor (>0) es primo
-}     
esPrimo :: Int -> Bool
esPrimo n = divisores n == 2

{-
Devuelve la cantidad de divisores para un número n entre 1 y n
-}
divisores :: Int -> Int
divisores n = divisores' n n

{-
Devuelve la cantidad de divisores para un número n entre 1 y k
-}
divisores' :: Int -> Int -> Int
divisores' n 1 = 1
divisores' n k | (n `mod` k) == 0 = 1 + divisores' n (k-1)
               | otherwise = divisores' n (k-1)




--Ejercicios del TP

--Ejercicio 1

merch :: [Int] -> [Int] -> [Int]
merch xs [] = xs 
merch [] ys = ys
merch (x:xs) (y:ys)
                    | x > y = [y] ++ merch (x:xs) (ys)  
                    | otherwise = [x] ++ merch (xs) (y:ys)
                    

--Ejercicio 2
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = menoresPivote ++ [x] ++ mayoresPivote
               where menoresPivote = ordenar [y | y <- (xs), y <= x]
                     mayoresPivote = ordenar [y | y <- (xs), y > x]


--Ejercicio 3
elevada :: Int -> Int
elevada 0 = 1
elevada 1 = 2
elevada n = 2 * elevada(n-1)

--Ejercicio 4
repreBinaria :: Int -> [Int]
repreBinaria 1 = [1]
repreBinaria n = (repreBinaria division) ++ [resto]
            where division = n `div` 2
                  resto = n `mod` 2

--Ejercicio 5
parBinario :: Int -> Bool
parBinario n | n `mod` 2 == 0 = True
             |otherwise = False

--Ejercicio 6

cuadradoPerfecto :: Int -> Bool
cuadradoPerfecto n |existe == [] = False
                   |otherwise = True
                 where existe = [x | x <- [1..n], x^2 == n]

--Ejercicio 7
repetidos :: a -> Int -> [a]
repetidos e 0 = []
repetidos e n = [e] ++ repetidos e (n-1)

--Ejercicio 8
nelem :: [a] -> Int -> a
nelem [] _ = error "lol"
nelem (x:xs) n = head (drop n (x:xs)) 

--Ejercicio 8 mejor hecho!
nelem' :: [a] -> Int -> a
nelem' [] _ = error "Lista Vacia!!!"
nelem' (x:xs) 0 = x
nelem' (x:xs) n = nelem' xs (n-1)







percent :: Int -> Int -> Float
percent x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float


compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100
