module Main where

import System.Environment
import Data.Char
import BolsaLog
import Tokeniser


main :: IO ()

main = do 
              args <- getArgs
              listaPalabras <- readFile (head args)
              putStr (cuentaPalabras listaPalabras)



--------------------------------------------------------------------------

-- código limpio

cuentaPalabras :: String -> String 
cuentaPalabras st = histograma . listaABolsa  $ tokens isAlpha st

histograma :: Bolsa String -> String
histograma b = concat . map (flip pintaLinea (palabraMasLarga b)) . bolsaALista $ b

pintaLinea :: (String, Int) -> Int -> String
pintaLinea (st,n) m = st ++ replicate (m - (length st)) ' ' ++ "[" ++ show n ++ "] : " ++ replicate n '*' ++ "\n"

palabraMasLarga :: Bolsa String -> Int
palabraMasLarga = foldBolsa f 0
                                    where 
                                        f   ni st _ nd = max (length st)  (max ni nd)