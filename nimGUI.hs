import AP.GUI
import System.Random
import Data.List







main :: IO ()

main = gui prog
	where
		prog = do
			w <- window [ title =: "NIM"
						, on windowClose =: quit
						]
			tablero <- frame [ size =: sz 680 420 ] w
			params <- frame [] w
			pilas <- label [ text =: "Introduzca número de pilas:" ] w
			entryPilas <- entry [ width =: 5
			 			   	, value =: "4"
							] w
			fichas <- label [ text =: "Introduzca número máximo de fichas:"] w
			entryFichas <- entry [ width =: 5
							, value =: "4"
							] w
			botonDibujar <- button [ text =: "Dibujar" 
									, on action =: do
													deshab [entryPilas, entryFichas]
													temp <- entryPilas ?: value
													let numPilas = read temp
													temp2 <- entryFichas ?: value
													let numFichas = read temp2
													listaPilas <- generaPilas numPilas numFichas
													let listaFichas = map (expande numFichas) listaPilas
													pintaTablero tablero numFichas (concat $ transpose listaFichas)
									] w
			params !: layout =: pilas <.< entryPilas <.< hSpace 40 <.< fichas <.< entryFichas <.< hSpace 40 <.< botonDibujar
			w !: layout =: (flexible params) ^.^ tablero				 
	
	
	
	
deshab :: (Able a) => [a] -> IO ()
deshab [] = return ()
deshab (x:xs)= do
			 	x !: enabled =: False
				deshab xs

pintaTablero :: Frame -> Int -> [Bool] -> IO ()
pintaTablero panel max lista = do
								let listaAcciones = [ accion panel x | x <- lista]
								listaCanvas <- sequence listaAcciones
								panel !: layout =: matrix max listaCanvas
						
generaPilas :: Int -> Int -> IO [Int]
generaPilas p n 
				| p == 0 = do 
							return []
				| otherwise = do 
								cola <- generaPilas (p-1) n
								aleatorio <- (randomRIO (1,n)) 
								return $ aleatorio : cola
								

expande :: Int -> Int -> [Bool]
expande n m = [ True | _ <- [1..n]] ++ [False | _ <- [(n+1)..m]]

accion :: Frame -> Bool -> IO Canvas
accion panel elem = do
						lienzo <- canvas [ bgColor =: white, size =: sz 50 50] panel
						if elem == True
							then do 
								oval (sz 40 40) [fillColor =: blue, position =: pt 25 25] lienzo
								return lienzo
							else do
								return lienzo
								
								
								
								
prueba = gui prog
			where 
				prog = do
						w <- window [ on windowClose =: quit ]
						f <- frame [] w
						lienzo <- accion f False 
						f !: layout =: lienzo
						w !: layout =: f
						