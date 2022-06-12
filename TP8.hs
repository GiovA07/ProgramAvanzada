{-TRABAJO PRACTICO NUMERO 8 DE PROGRAMACION AVANZADA-}


--Ejercicio 1. Definir la funcion nand a b = not (a && b) en Haskell sin utilizar not y &&.
nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

{-Ejercicio 2. Definir en Haskell la funcion 
                            maj : : Bool => Bool => Bool => Bool
                            True sii al menos 2 argumentos son True  -}   
maj:: Bool -> Bool -> Bool -> Bool
maj x y z = x && y || x && z || y && z

--lista por comprension
maj' :: Bool -> Bool -> Bool -> Bool
maj' x y z = length ([True | x <- [x,y,z], x == True]) == 2
--maj' x y z = elem True ( [ k && j | k <- [x,y,z], j <- [x,y,z], k == j ] )  
--maj' x y z = elem True ( [k && j | k <- [x,y,z], j <- [x,y,z], k /= j ])

-- EJercicio 3. Utilizar estas ideas para escribir los siguientes cuantificadores: (∃i : 0 ≤ i < ]xs : p xs.i)     (∀i : 0 ≤ i < ]xs : p xs.i)
-- Para un predicado p dado.

--paraTodo :: [a] -> (a -> Bool) -> Bool
--paraTodo [] _ = True
--paraTodo xs p = and [p x | x <- xs]

--length [x | x <- elementos, p x] == length xs
--length (filter p xs) == length xs


--Otra forma
paraTodo :: [a] -> (a -> Bool) -> Bool
paraTodo [] _ = True
paraTodo xs p = and [p x | x <- xs]



existe :: [a] -> (a -> Bool) -> Bool
existe xs p = or [p x | x <- xs] 


-- or [True | x <- xs, p x]
-- Si utilizo filter tenemos que comprobar que sea por lo menos 1
         --null  [True | x <- xs, p x]



--Ejercicio 4 Utilizando las ideas asociadas a listas por comprension, y las funciones sum, product, y length, escribir los cuantificadores de sumatoria, productoria y contatoria para ejemplos concretos.

--Sumatoria para los n primeros numeros pares.
sumatoria' :: [Int] -> Int
sumatoria' xs = sum ([x | x <- xs, (even x)])

--Productoria de todos los numeros pares.
productoria :: [Int] -> Int
productoria xs = product ([x | x <- xs, (even x)])

--Devolver Cantidad de numeros pares
contatoria :: [Int] -> Int
contatoria xs = length ([x | x <- xs, (even x)])
