module Main where
	
	import System.Environment
	import System.Exit
	import Data.Char
	import Data.Bits
	import EntradaSalida (pregunta)
	
	
	main :: IO()
	
	main = do
			args <- getArgs
			if (length args) < 4
				then 
					do
						putStrLn "Uso: cripto fichorigen fichdestino clave(0-255) opcion(e/d)"		
				else 
					do
						let
							fichorigen = head args
							fichdestino = head . tail $ args
							temp = read . head . tail . tail $ args
							opcion = head . tail . tail . tail $ args
							clave = temp `mod` 255
						print clave
						origen <- readFile fichorigen
							`catch`
								\_ -> do
										putStrLn "ERROR. El fichero origen no existe. "
										exitFailure
						if opcion == "e"
							then writeFile fichdestino $ encripta clave origen
							else  if opcion == "d"
								then writeFile fichdestino $ desencripta clave origen
								else putStrLn "ERROR. Opción incorrecta."

------------- Codigo puro


	encripta :: Int -> String -> String
	encripta clave = map  (chr . (xor clave) . ord) 

	desencripta :: Int -> String -> String
	desencripta = encripta   -- en este caso
	
	-- el algoritmo xor es muy débil, lo suyo sería implementar otro
	
