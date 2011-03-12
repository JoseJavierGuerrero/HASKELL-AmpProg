-------------------------------------------------------------------------------
-- TAD Bolsa (implementación lineal)
--
-- Autor:
-------------------------------------------------------------------------------

-- Interfaz

module BolsaLin ( Bolsa
                , vacia         -- Ord a => Bolsa a
                , insertar      -- Ord a => a -> Bolsa a -> Bolsa a
                , listaABolsa   -- Ord a => [a] -> Bolsa a
                , bolsaALista   -- Ord a => Bolsa a -> [(a, Int)]
                , tamaño        -- Ord a => Bolsa a -> Int
                , apariciones   -- Ord a => a -> Bolsa a -> Int
                , todos         -- Ord a => ( a-> Bool) -> Bolsa a -> Bool
              ) where

-- Implementacion

import Test.QuickCheck
import Data.Char (isUpper)
import Data.List (sort)

type Bolsa a = [(a, Int)] -- mantenemos la lista ordenada

-- define las operaciones del interfaz

-- ...

vacia :: Ord a => Bolsa a
vacia = []

insertar :: Ord a => a -> Bolsa a -> Bolsa a
insertar e [] = [(e,1)]
insertar e ys@((x,n):xs) 
              | e == x         = (x ,n+1) : xs
              | e > x           =  (x ,n) : insertar e xs
              | otherwise     = (e,1) : ys

listaABolsa :: Ord a => [a] -> Bolsa a
listaABolsa [] = vacia
listaABolsa (e : xs) = insertar e (listaABolsa xs)

bolsaALista :: Ord a => Bolsa a -> [(a,Int)]
bolsaALista = id

--- No es este el que piden pero aqui esta
bolsaALista' :: Ord a => Bolsa a -> [a]
bolsaALista' [] = []
bolsaALista' ((x,n):xs)
                  | n == 1              = x : ( bolsaALista' xs )
                  | otherwise          = x : ( bolsaALista' ( (x, n-1) : xs ) )

tamaño :: Ord a => Bolsa a -> Int
tamaño [] = 0
tamaño ((x,n):xs) = n + tamaño xs

apariciones :: Ord a => a -> Bolsa a -> Int
apariciones _ [] = 0
apariciones e ys@((x,n):xs) 
              | e == x           = n
              | e > x             = apariciones e xs
              | otherwise       = 0


todos :: Ord a => (a -> Bool) -> Bolsa a -> Bool
todos _ [] = True
todos f ((x,n):xs) = (f x) && (todos f xs)

 
-- Pruebas QuickCheck

-- tamaño funciona correctamente

prop_tamaño xs = tamaño (listaABolsa xs) == length xs
          where 
                  types = xs::String
                

-- apariciones funciona correctamente
 prop_apariciones xs = 


--Numero de ocurrencias en una lista

ocurrencias :: a -> [a] -> Int
ocurrencias x xs = length [_| y<-xs,x==y]


-- las listas devueltas por bolsaALista están ordenadas

-- prop_listaOrdenada xs  =



-- si se expande una bolsa en una lista con repeticiones [a],
-- se obtiene una ordenación de la lista original

-- prop_expande xs =



-- las representación interna de las bolsas está ordenada

-- prop_ordenInterno xs  =
