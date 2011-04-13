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
					juego pilas
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
								
								
juego :: [Int] -> IO ()

juego pilas = do
	 			pinta_pila pilas
				pilaj1 <- pregunta "Jugador 1: Número de pila: "
				fichasj1 <- pregunta "Jugado 1: Número de fichas a retirar: "
				let 
					pila1 = actualiza_pila pilas pilaj1 fichasj1
				if terminado pila1
					then 
						do
							putStrLn"Jugador 1 ha ganado el juego"
					else
						do
							pinta_pila pila1
							pilaj2 <- pregunta "Jugador 2: Número de pila:"
							fichasj2 <- pregunta "Jugador 2: Número de fichas a retirar: "
							let 
								pila2 = actualiza_pila pila1 pilaj2 fichasj2
							if terminado pila2
								then 
									putStrLn"Jugador 1 ha ganado el juego"
								else
									juego pila2 
									
terminado :: [Int] -> Bool
terminado = foldr (\x y -> (x==0) && y) True

actualiza_pila :: [Int] -> Int -> Int -> [Int]
actualiza_pila 	pila 1 fichas = if fichaspila < fichas
											then 
												0 : (tail pila)
											else 
												(fichaspila - fichas) : (tail pila)
										where 
											fichaspila = head pila
actualiza_pila pila n fichas = (head pila) : (actualiza_pila (tail pila) (n-1) fichas)

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
							