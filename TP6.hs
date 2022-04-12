--TP6

{-

DEFINICIONES DE TIPO EN HASKELL
UN tipo se define como:

data nombre = (lista de constructores)





--UN ejemplo simple podria ser:
--data VariableEntera = Variable Int
--Otra forma de crearlo puede ser:
data VariableEntera = Variable (valor :: Int)


variablePositiva :: Variable -> Bool
variablePositiva (Variable n) = n >= 0

valorDeVariable :: Variable -> Int
valorDeVariable (Variable n) = n



data listaEntera = Vacia | Cons Int ListaEntera deriving (Show, Eq)


data Booleano = Verdadero | Falso deriving (Show, Eq)

-}


data Quizas a = Nada | Justo {val::a} deriving (Show, Eq)

elementoEn :: [Int] -> Int -> Quizas Int
elementoEn [] _ = Nada
elementoEn (x:_) 0 = Justo x
elementoEn (x:xs) n | n < 0 = Nada
                    | otherwise = elementoEn xs (n-1)





--ARBOLESS

{-
data AB a = Vacio | AB a (AB a) (AB a) deriving (Show, Eq)

antecesor :: (Eq e) =>  e -> AB e -> Quizas e
antecesor _ Vacio = Nada
antecesor elem ab@(AB raiz ramaI ramaD) = antecesor' elem ab Nada


antecesor' :: (Eq e) => e -> AB e -> Quizas e -> Quizas e
antecesor' _ Vacio _ = Nada
antecesor'  elem (AB raiz ramaI ramaD) ant | elem == raiz = ant
                                             | antI /= Nada = antI
                                             | antD /= Nada = antD
                                             | otherwise = Nada
                                             where antI = antecesor' elem ramaI (Justo raiz)
                                                   antD = antecesor' elem ramaD (Justo raiz)

-}

{--Ejemplo:
> antecesor 2 (AB 1 (AB 2 Vacio Vacio) Vacio)

tiene que devolver el antecesor de la rama que tenga el "2". Es decir devolvera 1.

-}



-- Ejercicio 1. Definir el tipo Nat, visto en el teorico.

data Nat = Zero | Suc Nat deriving (Show, Eq)

-- Ejercicio 2. Definir la funcion natToInt : Nat → Int que dado un numero Nat retorna su entero correspondiente.
--  Por ejemplo: natToInt (Suc(Suc Zero)) = 2.

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc n) = 1 + (natToInt n)


--Ejercicio 3. Definir la funcion intToNat : Int → Nat que dado un numero entero retorna su Nat correspondiente. 
-- Por ejemplo: intToNat 2 = (Suc(Suc Zero))

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n-1))


--4. Definir la funcion sumaNat : Nat → Nat → Nat, la cual suma dos numeros Nat.
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero Zero = Zero
sumaNat (Suc e) Zero = (Suc e)
sumaNat Zero (Suc n) = (Suc n)
sumaNat (Suc e) (Suc n) = Suc (sumaNat e (Suc n))


{-Otra forma de definirlo-}
sumaNat' :: Nat -> Nat -> Nat
sumaNat' Zero Zero = Zero
sumaNat' (Suc e) Zero = (Suc e)
sumaNat' (Suc e) (Suc n) = Suc (sumaNat' e (Suc n))
sumaNat' Zero (Suc n) = Suc (sumaNat' Zero n)


--Ejercicio 5. Definir los  ́arboles binarios
data AB a = Vacio | Arbol a (AB a) (AB a) deriving (Show)

--Instance en HASKELL

instance (Eq a) => Eq (AB a) where
     (==) (Vacio) (Vacio) = True
     (==) (Arbol raiz ramaI ramaD) Vacio = False
     (==) Vacio (Arbol raiz ramaI ramaD) = False
     (==) (Arbol raiz' ramaI' ramaD') (Arbol raiz ramaI ramaD) = raiz == raiz' && ramaI == ramaI' && ramaD == ramaD'
     
     

 


--Ejercicio 6. La funcion size, que dado un  ́arbol retorna el numero de nodos del  arbol.
sizeArbol :: AB a -> Int
sizeArbol Vacio = 0
sizeArbol (Arbol raiz ramaI ramaD) = 1 + sumaRamaI + sumaRamaD
                                  where sumaRamaI = sizeArbol ramaI
                                        sumaRamaD = sizeArbol ramaD

--Ejercicio 7. La funcion height, que dado un arbol retorna la altura del mismo.
alturaArbol :: AB a -> Int
alturaArbol Vacio = 0
alturaArbol (Arbol raiz ramaI ramaD) = 1 + max (alturaArbol ramaI) (alturaArbol ramaD)




--Ejercicio 8 La funcion reverseTree, que dado un arbol retorna su arbol inverso
reverseTree :: AB a -> AB a 
reverseTree Vacio = Vacio
reverseTree (Arbol raiz ramaI ramaD) = (Arbol raiz reverseRamaD  reverseRamaI)
                                    where reverseRamaD = reverseTree (ramaD)
                                          reverseRamaI = reverseTree (ramaI)


{-
data AB a = Vacio | AB a (AB a) (AB a) deriving (Show, Eq)

antecesor :: (Eq e) =>  e -> AB e -> Quizas e
antecesor _ Vacio = Nada
antecesor elem ab@(AB raiz ramaI ramaD) = antecesor' elem ab Nada


antecesor' :: (Eq e) => e -> AB e -> Quizas e -> Quizas e
antecesor' _ Vacio _ = Nada
antecesor'  elem (AB raiz ramaI ramaD) ant | elem == raiz = ant
                                             | antI /= Nada = antI
                                             | antD /= Nada = antD
                                             | otherwise = Nada
                                             where antI = antecesor' elem ramaI (Justo raiz)
                                                   antD = antecesor' elem ramaD (Justo raiz)

-}




























