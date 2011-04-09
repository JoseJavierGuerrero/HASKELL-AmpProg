module Main where 


import System.Environment
import Data.Char
import EntradaSalida (pregunta)
import System.Random


main :: IO ()

main = do
		args <- getArgs
		if not $ null(args)
			then 
				do	
					let limites = (read $ head args, read $ head $ tail args) 
					secreto <- randomRIO limites
					adivina secreto
			else 
				do
					secreto <- randomRIO (1, 100) 
					adivina secreto
		return ()
		
		
		
adivina :: Int -> IO ()
adivina s = do
				intento <- pregunta "Venga, intentalo: "
				if s == intento
					then 
						do 
							putStrLn "Bien! Has adivinado el numero"
					else 
						if intento < s
							then
								do 
									putStrLn "Uyyyy, te has quedado corto..."
									adivina s
							else 
								do 
									putStrLn "Uyyyy, te has pasado..."
									adivina s
				
									
