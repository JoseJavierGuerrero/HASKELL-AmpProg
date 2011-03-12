module UsaPila () where

import Pila
import Data.Char (ord)

-- Tipo transparente: recursión y patrones

--{--

--listaAPila :: [a] -> Pila a

-- pilaALista :: Pila a -> [a]

-- profundidad :: Pila a -> Integer

-- ordinales :: Pila Char -> Pila Int

-- esta :: Eq a => a -> Pila a -> Bool

-- capicua :: String -> Bool

-- iguales :: Eq a => Pila a -> Pila a -> Bool


--}

-- Tipo Abstracto:  mapPila y foldPila

listaAPila' :: [a] -> Pila a
listaAPila' = foldr apilar vacia

pilaALista' :: Pila a -> [a]
pilaALista' = foldPila (:) []

prop_inversas xs = 
    pilaALista' (listaAPila' xs) == xs
        where 
              types = xs::String     -- para evitar que chequee con el tipo unit que siempre dara exito


profundidad' :: Pila a -> Int
profundidad' = foldPila (const (+1))  0

prop_profOK xs = 
    profundidad' (listaAPila' xs) == length xs
        where
              types = xs :: String

ordinales' :: Pila Char -> Pila Int
ordinales' = mapPila ord

esta' :: Eq a => a -> Pila a -> Bool
esta' a = foldPila f False
  where 
          f cab solCola = (cab == a) || solCola

capicua' :: String -> Bool
capicua' ss = reverse ss == ss

iguales' :: Eq a => Pila a -> Pila a -> Bool
iguales' p1 p2 
            | esVacia p1  && esVacia p2 = True
            | esVacia p1  = False
            | esVacia p2  = False
            | cima p1 == cima p2 = iguales' (desapilar p1) (desapilar p2)
