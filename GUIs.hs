import AP.GUI
import System.Random


ventanaDurmiente segs = gui prog
				where
					prog = do
							window [title =: "Una Ventana"
							 	   	,bgColor =: red
									]
							update
							sleep (1000 * segs)
							quit
							
							
							
--Bucea mucho tio.

--Ej 1 -> Buscar informacion sobre el atributo Position => Positioned


ventanaMutable segs = gui prog
		where 
			prog = do
					w <- window [title =: "Una ventana"
								, bgColor =: red
								]
					t <- w ?: title -- preguntando por el valor del atributo title de w
					putStrLn t
					update
					sleep (1000 * segs)
					w !!: 	[title =: "Nuevo titulo"  -- forzamos cambio de titulo de w, con !: prop unica
							, bgColor =: blue
							]
					update
					t2 <- w ?: title -- preguntando por el valor del atributo title de w
					putStrLn t2
					quit
					
					
ventanaMutable2 segs = gui prog
		where 
			prog = do
					w <- window [title =: "Ventana grande"
								, bgColor =: blue
								, size =: Size 800 600
								]
					s <- w ?: size
					putStrLn $ show s
					update 
					sleep (1000 * segs)
					w !!:   [title =: "Ventana pequeña"
							, bgColor =: orange
							, size =: Size 200 100
							]
					update
					s2 <- w ?: size
					putStrLn $ show s2
					sleep (1000 * segs)
					quit
					 
					
ventanaDespierta = gui prog
			where
				prog = do
						w <- window [title =: "Despierta"
									, bgColor =: blue
									, size =: Size 100 200
									, on windowClose =: quit]
						return ()
						
						
ventanaEventos = gui prog 
			where
				prog = do
						w <- window [title =: "Eventos"
									, bgColor =: orange
									, size =: Size 400 200
									, on windowClose =: quit
									]
									
						w !!: [on focusGain =: do 
												w !: bgColor =: yellow
							  ,on focusLoose =: do
												w !: bgColor =: black
							  ,on mouseEnter =: do
												w !: size =: Size 800 600
							  ,on mouseExit =: do
												w !: size =: Size 200 400
							  ]
							
							

									
									


------------ Ejercicio 7 ---------------
ventanaAleatoria :: String -> IO Window 

ventanaAleatoria titulo = do
							sizeX <- randomRIO (0, 800)
							sizeY <- randomRIO (0, 800)
							colorR <- randomRIO (0,255)
							colorG <- randomRIO (0,255)
							colorB <- randomRIO (0,255)
							posX <- randomRIO (0, 800)
							posY <- randomRIO (0, 800)
							w <- window [title =: titulo
										  , position =: pt posX posY
								 		  , bgColor =: rgb colorR colorG colorB
								 		  , size =: Size sizeX sizeY
								 		  ]
							w !: on windowClose =: close w
							return w
								

------------ Ejercicio 8 ----------------			
									
collage :: Int -> IO ()
collage n = gui prog
				where 
					prog = do
								m <- randomRIO (1,n)
								let listaVentanas = [ventanaAleatoria $ "Ventana " ++ show x | x <- [1..n], x /= m]
								vQuit <- ventanaAleatoria $ "Ventana " ++ show m
								vQuit !: on windowClose =: quit
								sequence_ listaVentanas
									

------------ Ejercicio 9 -----------------
ventanaAleatoriaCierre :: String -> IO ()
ventanaAleatoriaCierre titulo = gui prog
								where prog = do
									ventana <- ventanaAleatoria titulo
									do
										suerte <- randomIO
										if suerte 
											then 
												ventana !: on windowClose =: quit
											else 
												ventana !: on windowClose =: do
																		close ventana
																		prog
									return ()


------------ Controles -------------------

							
holaMundo = gui prog
		where
			prog = do
					w <- window [ title =: "Saludo desde GUI"
								, on windowClose =: quit
								]
					l <- label [text =: "¡Hola Mundo!"] w
					return ()

holaMundo2 = gui prog
		where
			prog = do
					w <- window [ title =: "Saludo desde GUI"
								, on windowClose =: quit
								]
					l <- label [text =: "¡Hola Mundo!"] w
					w !: layout =: l -- layout por defecto
					
					
holaMundo3 = gui prog
		where
			prog = do
					w <- window [ title =: "Saludo desde GUI"
								, on windowClose =: quit
								]
					l <- label [text =: "¡Hola Mundo!"] w
					w !: layout =: east l -- west, north, south
					
			
			
holaMundo4 = gui prog
		where
			prog = do
					w <- window [ title =: "Saludo desde GUI"
								, on windowClose =: quit
								]
					l <- label [text =: "¡Hola Mundo!"
								, font =: (bold . italic . arial) 26
								, color =: yellow
								, bgColor =: blue
								] w
					w !: layout =: west (attachBottom l) 
										
					
holaAdios = gui prog
		where
			prog = do
					w <- window [ title =: "Saludo desde GUI"
								, on windowClose =: quit
								]
					hola <- label [text =: "¡Hola Mundo!"
								, font =: (bold . italic . arial) 26
								, color =: yellow
								, bgColor =: blue
								] w
					adios <- label [text =: "¡Hola Mundo!"
								, font =: (italic . arial) 26
								, color =: green
								, bgColor =: red
								] w
					w !: layout =: hola ^.^ flexible (space (sz 10 10)) ^.^ adios





------------ Ejercicio 10 ----------------


holaAdios' = gui prog
		where
			prog = do
					w <- window [ title =: "Saludo desde GUI"
								, on windowClose =: quit
								]
					hola <- label [text =: "¡Hola Mundo!"
								, font =: (bold . italic . arial) 26
								, color =: yellow
								, bgColor =: blue
								, relief =: Sunken
								, borderWidth =: 10
								] w
					adios <- label [text =: "¡Hola Mundo!"
								, font =: (italic . arial) 26
								, color =: green
								, bgColor =: red
								, relief =: Ridge
								, borderWidth =: 5
								] w
					w !: layout =: hola ^.^ flexible (space (sz 10 10)) ^.^ adios



------------ Ejercicio 11 -----------------



pintaMatriz :: [String] -> Int -> IO ()
pintaMatriz listaTitulos ancho = gui prog
					where 
						prog = do
							w <- window [ title =: "Una Matriz"
										, bgColor =: white
										, on windowClose =: quit	
										]
							let listaAcciones = [boton w titulo | titulo <- listaTitulos]
							listaControles <- sequence listaAcciones
							w !: layout =: matrix ancho listaControles	
					


boton :: Window -> String -> IO Label
boton w titulo = do 
			etiqueta <- label [ text =: titulo
							  , font =: (bold . arial) 16 
							  , color =: black
							  , bgColor =: grey
							  , relief =: Raised
							  , borderWidth =: 4
							  , size =: Size 4 3
							  ] w
			return etiqueta


botonesCalculadora :: [String]
botonesCalculadora = ["C","+/-","/","x","7","8","9","-","4","5","6","+","1","2","3","=","0","00",",","%"]


calculadora = pintaMatriz botonesCalculadora 4


------------  Eventos de teclado y ratón -----------------

logPulsaciones = gui prog
					where 
						prog = do
								w <- window [ title =: "Log de botones del ratón"
											, on windowClose =: quit
											]
								w !: on mousePress =: procesaRaton
								
procesaRaton :: MouseEvent -> IO ()
procesaRaton r = putStrLn $ boton ++ "(" ++ x ++ ", " ++ y ++")"
					where 
						boton = show . mouseButton $ r
						x = show . pointX . mousePoint $ r
						y = show . pointY . mousePoint $ r
						
						
---- modificadores de evento de ratón: left, right, middle, double, triple

logRatIzq = gui prog
		where 
			prog = do 
				w <- window [ title =: "Log de doble pulsación izquierda"
							, on windowClose =: quit
							]
				w !: on (left mousePress) =: const procesaSimple
				w !: on (double (left mousePress)) =: const procesaDoble
				
procesaSimple :: IO()
procesaSimple = putStrLn "¡Simple!"

procesaDoble :: IO ()
procesaDoble = putStrLn "¡Doble!"




----------- Ejercicio 12 ------------


estadoRaton = gui prog 
				where 
					prog = do 
							w <- window [ title =: "Ventana de estado del ratón"
										, on windowClose =: quit
										]
							etPosRaton <- label [ text =: "Hola!"
												, justify =: JustifyCenter
												, bgColor =: green
												, color =: blue
												] w
							w !: on mouseExit =: do 
								 					etPosRaton !!: [ text =: "El cursor está fuera!"
																   , bgColor =: red
																   , color =: black
																   ] 
							w !: on mouseEnter =: do
													etPosRaton !!: [ text =: "El cursor está dentro!"
																   , bgColor =: green
																   , color =: blue
																   ] 
							w !: on mouseMove =: pintaPosicion etPosRaton
							w !: on mousePress =: const (do
													etPosRaton !: text =: "¡Simple!")
							w !: on (double mousePress) =: const (do
															etPosRaton !: text =: "¡Doble!")
							w !: layout =: (fillX (attachBottom  etPosRaton))
							

estadoRaton' = gui prog 
				where 
					prog = do 
							w <- window [ title =: "Ventana de estado del ratón"
										, on windowClose =: quit
										]
							etPosRaton <- label [ text =: "Hola!"
												, justify =: JustifyCenter
												, bgColor =: green
												, color =: blue
												] w
							w !: on mouseExit =: do 
								 					etPosRaton !: text =: "El cursor está fuera!"
													w !: bgColor =: red
							w !: on mouseEnter =: do
													w !: bgColor =: green		
							w !: on mouseMove =: pintaPosicion etPosRaton
							w !: on mousePress =: const (do
													etPosRaton !: text =: "¡Simple!")
							w !: on (double mousePress) =: const (do
															etPosRaton !: text =: "¡Doble!")
							w !: layout =: (fillX (attachBottom  etPosRaton))
							
							
pintaPosicion :: Label -> Point -> IO ()
pintaPosicion e r = e !: text =: posicion
			where
				posicion = "posX : " ++ x ++ " | posY: " ++ y
				x = show . pointX $ r
				y = show . pointY $ r
								
								
--------- Ejercicio 13 ------------

logTeclas = gui prog 
		where 
			prog = do
				w <- window [ title =: "Log de pulsación de teclas"
							, bgColor =: grey
							, on windowClose =: quit
							]
				info <- label [ text =: " Pulsa alguna tecla! "
							  , bgColor =: black
							  , color =: white
							  , justify =: JustifyCenter
							  ] w
				w !: on keyPress =: (\t -> do {info !: text =: show t}) --- leeTecla info
				w !: layout =: flexible info
				
leeTecla :: Label -> KeyEvent -> IO ()
leeTecla l t = l !: text =: infoTecla
					where
						infoTecla = "Nombre: " ++ nombre ++ " Carácter: " ++ car ++ " Código: " ++ codigo
						nombre = keyText $ t
						car = show . keyChar $ t
						codigo = show . keyCode $ t
												
------- Controles Interactivos ----------

encadenaDialogos :: IO ()
encadenaDialogos = gui prog 
		where
			prog = do
				w <- window [ title =: "Ventana padre"
							, on windowClose =: quit
							]
				q <- button [ text =: "Abandonar"
							, on action =: quit
							] w
				b <- button [ text =: "Nuevo Diálogo"
							, on action =: creaDialogo w 1
							] w
				w !: layout =: b <.< q
			
creaDialogo :: Container w => w -> Int -> IO()
creaDialogo w n = do
				d <- dialog [ title =: "Dialogo " ++ show n
							] w
				b1 <- button [ text =: "Cerrar"
							 , on action =: close d
							 ] d
				b2 <- button [ text =: "Nuevo Dialogo"
							 , on action =: creaDialogo d (n+1)
							 ] d
			 	q <- button [ text =: "Abandonar"
							 , on action =: quit
							 ] d
				d !: layout =: b1 <.< b2 <.< q
							
------ Ventana de Login ------

login = gui prog
   where 
	prog = do 
		w <- window  [ title =: "Inicio de Sesión"
				     , resizable =: False
					 , on windowClose =: quit
		 			 ]
		ln <- label [ text =: "Nombre" ] w
		n <- entry [ value =: "escriba su nombre"
				   , font =: arial 14
				   ] w
		lc <- label [ text =: "   Clave"] w
		c <- entry [ value =: "escriba su clave"
				   , font =: arial 14
				   , password	 =: True
				   ] w
		ocultar <- checkButton [ text =: "ocultar"
							   , checked =: True
							   , on action =: (c !: password <: not)
							   ] w
		iniciar <- button [ text =: "Iniciar"
			 			  , on action =: do {imprime n c; quit}
						  ] w
		limpiar <- button [ text =: "Limpiar"
						  , on action =: limpia n c
						  ] w
		cancelar <- button [ text =: "Cancelar"
						   , on action =: quit
						   ] w
		w !: layout =: west (ln <.< n) ^.^
					   west (lc <.< c <.< ocultar) ^.^
					   iniciar <.< hSpace 20 <.< limpiar <.< hSpace 20 <.< cancelar




imprime :: Entry String -> Entry String -> IO ()
imprime nombre clave = do 
				usr <- nombre ?: value
				pass <- clave ?: value
				putStrLn $ "usuario: " ++ usr
				putStrLn $ "clave: " ++ pass
				
limpia :: Entry String -> Entry String -> IO ()
limpia nombre clave = do 
			nombre !: value =: ""
			clave !: value =: ""



--------- Ventana de edición -------------

----------- Ejercicio 15 --------------

correo = gui  prog
		where
			prog = do
				w <- window [ title =: "Correo electrónico"
							, resizable =: False
							, on windowClose =: quit
							]
			 	para <- label [ text =: "Para :" ] w
				direccion <- entry [ value =: ""
								   , width =: 45
								   ] w
				fpara <- frame [ layout =: para <.< (fillX direccion)] w
				cuerpo <- edit [ value =: ""
							   , font =: arial 14 
							   , wrap =: True
							   ] w
				wrap <- checkButton [ text =: "Wrap?"
									, checked =: True
									, on action =: (cuerpo !: wrap <: not)
									] w
				vDesp <- vScrollbar [] cuerpo
				hDesp <- hScrollbar [] cuerpo
				fcuerpo <- frame [layout =: (cuerpo ^.^ (fillX hDesp)) <.< (fillY vDesp) ] w
				enviar <- button [ text =: "Enviar"
								 , on action =: do
										to <- direccion ?: value
										putStrLn ("to:" ++ to)
										body <- cuerpo ?: value
										putStrLn "body:"
										putStrLn body
										quit
								 ] w
				cancelar <- button [ text =: "Cancelar"
								   , on action =: quit
								   ] w
				borrar <- button [ text =: "Borrar"
								 , on action =: do
											direccion !: value =: ""
											cuerpo !: value =: ""
								 ] w
				botonera <- frame [layout =: enviar <.< hSpace 20 <.< 
											 borrar <.< hSpace 20 <.<
											 cancelar <.< hSpace 250 <.<
											 wrap
								  ] w
				w !: layout =: west fpara ^.^
							   fcuerpo ^.^
							   (east botonera)
				
-------- Menus y cajas de dialogo -------

aplicacion = gui prog
				where
					prog = do 
						app <- window [ title =: "Aplicación"
										, size =: Size 500 300
										]
						app !: on windowClose =: quit
						
						barra <- menu []
						menuArchivo app barra
						menuEditar app barra
						menuAyuda app barra
						app !: menuBar =: barra
						app !: on (right mousePress) =: popupAt barra 
						
						
menuArchivo :: Window -> Menu -> IO Menu
menuArchivo app barra = do
			archivo <- submenu [ text =: "&Archivo" ] barra
			menuItem [ text =: "&Abrir"
					 , on action =: abrirFichero app
					 ] archivo
			menuItem [ text =: "&Cerrar"
					 , enabled =: False
					 ] archivo
			menuSep [] archivo
			menuItem [ text =: "&Salir"
					 , on action =: cerrarApp app
					 ] archivo
			return archivo
			
			
menuEditar :: Window -> Menu -> IO Menu 
menuEditar _ barra = do
			editar <- submenu [ text =: "&Editar" ] barra
			menuItem [ text =: "&Buscar" ] editar
			menuItem [ text =: "&Reemplazar" ] editar
			return editar
			
menuAyuda :: Window -> Menu -> IO Menu
menuAyuda app barra = do
	ayuda <- submenu [ text =: "A&yuda" ] barra
	menuItem [text =: "Acerca de"
			 , on action =: acercaDe app
			 ] ayuda
	return ayuda
	
abrirFichero :: Window -> IO ()
abrirFichero app = do
			ruta <- openFileDialog app
			procesaRuta ruta
			
procesaRuta Nothing = putStrLn "no se escogió ningún fichero"
procesaRuta (Just r) = putStrLn $ "se escogio el fichero " ++ r

cerrarApp :: Window -> IO ()
cerrarApp app = do
	 fin <- okCancelMessageBox app "Cerrar" "¿Está seguro?"
	 if fin then quit
			else return ()
			
acercaDe :: Window -> IO () 
acercaDe app = do 
			okMessageBox app "Acerca de ..." ("Ampliación de programación\n" ++
											 "GUIs en Haskell  con AP.GUI")
			return ()
			
-------- Cuenta atrás, temporizadores ----------											

-------- Ejercicio 17 --------------	
cuentaAtras1 :: Int -> IO ()  -- con label, read y show

cuentaAtras1 n = gui prog
					where prog = do
						let sec = n + 1
						w <- window [ title =: "Cuenta atrás"
									, on windowClose =: quit
									]
						contador <- label [ text =: show sec 
										  , font =: (bold . arial) 60
										  , justify =: JustifyCenter
										  ] w
						segundo <- timer [ interval =: 1000
										 , on action =: do 
													valorst <- contador ?: text
													let valorint = read (valorst) 
													if valorint == 0 
													  then 
														return ()
													  else 
														do 
															let nuevovalor = valorint - 1 
															contador !: text =:  show nuevovalor
										 ]
						w !: layout =: contador 
						return ()


cuentaAtras2 :: Int -> IO ()  -- con label, read y show

cuentaAtras2 n = gui prog
					where prog = do
						w <- window [ title =: "Cuenta atrás"
									, on windowClose =: quit
									]
						contInt <- var [value =: n+1]
						contador <- label [ text =: show (n+1)
										  , font =: (bold . arial) 60
										  , justify =: JustifyCenter
										  ] w
						segundo <- timer [ interval =: 1000
										 , on action =: do  
													sec <- contInt ?: value
													if sec == 0 
													  then 
														return ()
													  else 
														do 
															let nuevoSec = sec - 1
															contInt !: value =: nuevoSec 
															contador !: text =: show (nuevoSec)
										 ]
						w !: layout =: contador 
						return ()						
			



	
									