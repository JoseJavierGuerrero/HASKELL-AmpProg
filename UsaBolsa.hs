module Main(main) where

import BolsaLin

miBolsa :: Bolsa Char
miBolsa = listaABolsa "Ampliación de Programación"

frecuencias = bolsaALista . listaABolsa

main :: IO ()
main = do
         xs <- getLine
         print (frecuencias xs)
