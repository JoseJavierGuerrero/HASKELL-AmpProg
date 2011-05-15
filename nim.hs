module Main where

import System.Environment
import System.Random
import Data.Typeable
import EntradaSalida(pregunta)


	
main :: IO ()
main = do 
		args <- getArgs
		if (length args) == 2 
			then 
				do
					pilas <- genera_pilas (read . head $ args) (read . head . tail $ args) 	
					juego 0 pilas
			else
				do
					putStrLn "Uso: ./nim numeropilas maximo"

genera_pilas :: Int -> Int -> IO [Int]

genera_pilas p n 
				| p == 0 = do 
							return []
				| otherwise = do 
								cola <- genera_pilas (p-1) n
								aleatorio <- (randomRIO (1,n)) 
								return $ aleatorio : cola
								
								
juego :: Int -> [Int] -> IO ()

juego jugador pilas = do
						pinta_pila pilas
						putStrLn $ "Jugador " ++ show jugador ++ ":"
						jugada <- preguntas
						if valida jugada pilas
							then
							  let pila1 = actualiza_pila pilas jugada in
							  if terminado pila1
									then do {putStrLn $ "El jugador " ++ show jugador ++ " ha ganado la partida!"}
									else juego (1 - jugador) pila1
							else 
								do
									putStrLn "Jugada incorrecta! Vuelva a introducir su jugada:"
									juego jugador pilas
					
				
			

			
preguntas :: IO (Int,Int)
preguntas = do 
				numeropila <- pregunta "Número de pila: "
				fichas <- pregunta "Número de fichas: "
				return (numeropila,fichas)

				
valida :: (Int, Int) -> [Int] -> Bool
valida (n,m) pila
			| n <= (length pila) && fichasOK n m pila = True
			| otherwise = False

			
fichasOK :: Int -> Int -> [Int] -> Bool
fichasOK 1 m pila = m <= (head pila)
fichasOK n m pila = fichasOK (n-1) m (tail pila)

									
terminado :: [Int] -> Bool
terminado = foldr (\x y -> (x==0) && y) True


actualiza_pila :: [Int] -> (Int, Int) -> [Int]
actualiza_pila 	pila (1, fichas) =  (fichaspila - fichas) : (tail pila)
										where 
											fichaspila = head pila

actualiza_pila pila (n, fichas) = head pila : actualiza_pila (tail pila) (n-1, fichas)									
										
										
pinta_pila :: [Int] -> IO ()
pinta_pila p = pinta_aux p 1

pinta_aux :: [Int] -> Int -> IO ()
pinta_aux p n 
 			| p == [] = do 
							return ()
			| otherwise = do
							let asteriscos = replicate (head p) '*'
							putStrLn $ "["++show n++"]"++" : "++ asteriscos
							pinta_aux (tail p) (n+1)
							