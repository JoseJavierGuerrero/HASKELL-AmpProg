import AP.GUI


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
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									