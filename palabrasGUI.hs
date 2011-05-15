module Main where

import AP.GUI
import System.Environment
import System.Exit
import Data.Char
import BolsaLog
import Tokeniser


main :: IO ()

main = gui prog 
		where 
			prog = do
				w <- window [ title =: " Cuenta Palabras "
							, on windowClose =: quit
							]
				panel <- canvas [ bgColor =: white
									 , size =: sz 800 600
									 ] w
				estadisticas <- label [text =: "Pulse el botón \"Abrir Fichero\" para seleccionar el texto"
									  , justify =: JustifyCenter
									  ] w
				botonAbrir <- button [ text =: "Abrir Fichero"
									 , on action =: do
												 fichero <- abrirFichero w
												 if fichero /= ""
												 	then pintarHistograma fichero panel
													else return ()
									 ] w
				hDesp <- hScrollbar [] panel
				fBoton <- frame [layout =: hSpace 30 <.< (flexible botonAbrir) <.< hSpace 30] w
				w !: layout =: (fillX estadisticas) ^.^ (panel ^.^ (fillX hDesp)) ^.^ vSpace 20 ^.^ (fillX fBoton)	
				return ()
				
			
			
abrirFichero :: Window -> IO String
abrirFichero w = do
				ruta <- openFileDialog w
				texto <- procesaRuta ruta
				return texto
				
				
procesaRuta Nothing = return ""
procesaRuta (Just r) = do 
						texto <- readFile r
						return texto
						
pintarHistograma :: String -> Canvas -> IO ()
pintarHistograma texto panel = pintaPalabras (pt 80 0) panel listaFrecuencias separador
					where
						listaFrecuencias = bolsaALista bolsaPalabras
						bolsaPalabras = listaABolsa $ tokens isAlpha texto
						separador = (palabraMasLarga bolsaPalabras) * 8
						
						
pintaPalabras :: Point -> Canvas -> [(String, Int)] -> Int -> IO ()
pintaPalabras _ _ [] _ = return ()
pintaPalabras (Point x _) panel ((palabra,frec):xs) separador = do
											palabraPintada <- writing palabra [ color =: red
																			  , font =: (bold . arial) ((1 `div` length palabra)*20)
																			  , position =: pt x 580
																			  ] panel
											
											frecuencia <- writing (show frec) [ color =: red
																			  , font =: (bold . arial) 16
																			  , position =: pt x 300
																			  ] panel
											rectangFrec <- rectangle (sz 50 frec) [ bgColor =: blue
																				  , outlineWidth =: 1
																				  , outlineColor =: cyan
																				  , position =: pt x 300
																				  ] panel
											raise palabraPintada
											raise frecuencia
											lineaSep <- line [pt (x + separador `div` 2) 30, pt (x + separador `div` 2) 570 ] [] panel
											pintaPalabras (pt (x + separador) 0) panel xs separador


palabraMasLarga :: Bolsa String -> Int
palabraMasLarga = foldBolsa f 0
                                    where 
                                        f   ni st _ nd = max (length st)  (max ni nd)