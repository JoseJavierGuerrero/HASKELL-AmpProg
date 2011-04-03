-------------------------------------------------------------------------------
-- TAD Bolsa (implementación logarítmica)
--
-- Autor:
-------------------------------------------------------------------------------

-- Interfaz

module BolsaLog ( Bolsa
                , vacia         -- Ord a => Bolsa a
                , insertar      -- Ord a => a -> Bolsa a -> Bolsa a
                , listaABolsa   -- Ord a => [a] -> Bolsa a
                , bolsaALista   -- Ord a => Bolsa a -> [(a, Int)]
                , tamaño        -- Ord a => Bolsa a -> Int
                , apariciones   -- Ord a => a -> Bolsa a -> Int
                , todos         -- Ord a => ( a-> Bool) -> Bolsa a -> Bool
                , foldBolsa
             ) where

-- Implementacion

import Test.QuickCheck
import Data.Char (isUpper)
import Data.List (sort)

data Bolsa a = Vacia
             | Nodo (Bolsa a) a Int (Bolsa a) deriving Show

foldBolsa :: (b -> a -> Int -> b -> b) -> b -> Bolsa a -> b
foldBolsa f base = plegar
        where 
              plegar Vacia = base 
              plegar (Nodo i e n d) = f (plegar i) e n (plegar d) 
        

{-
foldBolsaAc :: (a -> Int -> b -> b) -> b -> Bolsa a -> b
foldBolsaAc f ini b = acumular b ini 
        where 
              acumular Vacia ac = ac
              acumular (Nodo i e n d) ac = 
-}


vacia :: Ord a => Bolsa a
vacia = Vacia

insertar :: Ord a => a -> Bolsa a -> Bolsa a
insertar x Vacia = Nodo Vacia x 1 Vacia
insertar x (Nodo i e n d) 
                  |  e == x    = Nodo i e (n+1) d
                  |  e > x      = Nodo (insertar x i) e n d
                  | otherwise = Nodo i e n (insertar x d)

listaABolsa :: Ord a => [a] -> Bolsa a
listaABolsa = foldr insertar Vacia

{-
listaABolsa [] = Vacia
listaABolsa (x:xs) = insertar x (listaABolsa xs)
-}

bolsaALista :: Ord a => Bolsa a -> [(a,Int)]
bolsaALista = foldBolsa (\w x y z -> w ++ [(x,y)] ++ z) []

-- Pruebas QuickCheck



tamaño :: Ord a => Bolsa a -> Int
tamaño = foldBolsa (\w _ y z -> w + y + z) 0
-- tamaño funciona correctamente

prop_tamaño xs = 
                length xs == tamaño (listaABolsa xs)
          where 
                types = xs :: String


apariciones :: Ord a => a -> Bolsa a -> Int
apariciones _ Vacia = 0
apariciones x (Nodo i e n d)
                      | x == e          = n
                      | x < e            = apariciones x i
                      | otherwise      = apariciones x d

-- apariciones funciona correctamente

prop_apariciones xs = 
          and . zipWith (==) cuantas_xs $ cuantas_bolsa xs
      where 
          bolsa = listaABolsa xs
          cuantas_bolsa xs = map (flip apariciones bolsa) xs
          cuantas ys x = length [1 | y <- ys, x == y]
          cuantas_xs = map (cuantas xs) xs
          types = xs :: String


todos :: Ord a => ( a-> Bool) -> Bolsa a -> Bool
todos f = foldBolsa (\w x y z -> w && (f x) && z) True



-- las listas devueltas por bolsaALista están ordenadas

-- prop_listaOrdenada xs  =



-- si se expande una bolsa en una lista con repeticiones [a],
-- se obtiene una ordenación de la lista original

-- prop_expande xs =



-- la representación interna de las bolsas está ordenada

-- prop_ordenInterno xs  =

-- las implementaciones alternativas son iguales...
