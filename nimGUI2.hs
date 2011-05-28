{- Jose J Guerrero Montero
    Ampliación de programación 2011

    Con un poco más de tiempo y si se esperara más de esta práctica
    habría sopesado la opción de crear tipo especificos para los jugadores (en vez de las tuplas)
    o incluso para los parámetros de entrada, pero he estimado que se sale de los objetivos 
    
-}



import AP.GUI
import System.Random
import Data.List
--import Hledger.Data.Utils para abrir el navegador


main :: IO ()

main = gui prog
    where 
        prog = do
            w <- window [ title =: "NIM"
                        , size =: sz 1024 768
                        , on windowClose =: quit
                        ]
            marco <- frame [ relief =: Sunken ] w
            tablero <- frame [ relief =: Raised ] w
            mensajes <- label [ text =: "Empieza la partida! Selecciona número de pilas y máximo de fichas"
                              , color =: blue
                              , bgColor =: white
                              , font =: ( bold . arial ) 16
                              , relief =: Ridge 
                              ] w
            let initVal = 4 :: Int
            labelPilas <- label [ text =: "Introduzca número de pilas:" ] w
            entryPilas <- entry [ width =: 5
                                , value =: initVal
                            ] w
            labelFichas <- label [ text =: "Introduzca número máximo de fichas:"] w
            entryFichas <- entry [ width =: 5
                            , value =: initVal
                            ] w
            listaPilas <- var []
            numFichas <- var []
            numPilas <- var []
            jugador1 <- frame [ relief =: Groove ] w
            jugador2 <- frame [ relief =: Groove ] w
            let initValue = 0 :: Int
            fichasJ1 <- entry [ value =: initValue ] jugador1
            fichasJ2 <- entry [ value =: initValue ] jugador2
            let props = [ text =: "Tomar!", font =: ( bold . italic . arial ) 20 ]
            botonJ1 <- button props jugador1
            botonJ2 <- button props jugador2
            radioGJ1 <- radioGroup []
            radioGJ2 <- radioGroup []
            let j1 = (fichasJ1,radioGJ1,botonJ1)
            let j2 = (fichasJ2,radioGJ2,botonJ2)
            botonDibujar <- button [ text =: "Dibujar"] w
            botonDibujar !: on action =: do
                                            tempPilas <- entryPilas ?: value
                                            numPilas !: value =: tempPilas
                                            tempFichas <- entryFichas ?: value
                                            numFichas !: value =: tempFichas
                                            tempListaPilas <- genera_pilas tempPilas tempFichas
                                            listaPilas !: value =: tempListaPilas
                                            deshab botonDibujar
                                            mapM deshab [entryPilas, entryFichas]
                                            nueva_partida listaPilas numFichas numPilas tablero jugador1 jugador2 j1 j2 mensajes
            let params = (entryPilas, entryFichas, botonDibujar)
            botonJ1 !: on action =: do
                            gameOver <- juego j1 j2 tablero mensajes listaPilas numFichas
                            if gameOver then ganador w "Jugador 1" params else return ()
                                        
            botonJ2 !: on action =: do
                                        gameOver <- juego j2 j1 tablero mensajes listaPilas numFichas
                                        if gameOver then ganador w "Jugador 2" params else return ()
            paramsF <- frame [ layout =: labelPilas <.< entryPilas 
                              <.< hSpace 40 <.< labelFichas <.< entryFichas 
                              <.< hSpace 40 <.< botonDibujar] w
            marco <- frame [ layout =:  vSpace 20
                                   ^.^ (jugador1 <.< hSpace 20 
                                   <.< tablero <.< hSpace 20 
                                   <.< jugador2)] w
            barra <- menu []
            menuArchivo w barra params mensajes j1 j2
            menuAyuda w barra 
            w !: menuBar =: barra
            w !: on (right mousePress) =: popupAt barra 
            w !: layout =: paramsF ^.^ flexible marco 
                            ^.^  (south . attachBottom . fillX $ mensajes)
    
----------------------------------------------------------------------------

-- Funciones relativas al juego en si --

nueva_partida :: Var [Int] -> Var Int -> Var Int -> Frame -> Frame -> Frame -> (Entry Int, RadioGroup, Button) -> (Entry Int, RadioGroup, Button) -> Label -> IO ()
nueva_partida listaPilas numFichas numPilas tablero jugador1 jugador2 
            j1@(fichasJ1,radioGJ1,botonJ1) j2@(fichasJ2,radioGJ2,botonJ2) mensajes = do
                                            pinta_tablero tablero listaPilas numFichas
                                            radioJ1 <- botones_r jugador1 numPilas 
                                            radioJ2 <- botones_r jugador2 numPilas
                                            radioGJ1 !: radioButtons =: radioJ1 
                                            radioGJ2 !: radioButtons =: radioJ2
                                            labelJ1 <- label [ text =: "JUGADOR 1\n\n¿Cuantas fichas?" ] jugador1
                                            labelJ2 <- label [ text =: "JUGADOR 2\n\n¿Cuantas fichas?" ] jugador2
                                            jugador1 !: layout =: labelJ1 ^.^ fichasJ1 ^.^ (vertical radioJ1) ^.^ botonJ1 ^.^ vSpace 5
                                            jugador2 !: layout =: labelJ2 ^.^ fichasJ2 ^.^ (vertical radioJ2) ^.^ botonJ2 ^.^ vSpace 5
                                            habJugador j1
                                            deshabJugador j2
                                            mensajes !!: [text =: "Empieza el jugador número 1!"
                                                         ,color =: green
                                                         ]

genera_pilas :: Int -> Int -> IO [Int]
genera_pilas p n 
                | p == 0 = do 
                            return []
                | otherwise = do 
                                cola <- genera_pilas (p-1) n
                                aleatorio <- (randomRIO (1,n)) 
                                return $ aleatorio : cola   

juego ::  (Entry Int, RadioGroup, Button) ->  (Entry Int, RadioGroup, Button) -> Frame -> Label -> Var [Int] -> Var Int -> IO Bool
juego jugando@(fichasJ1,radioGJ1,botonJ1) contrario@(fichasJ2,radioGJ2,botonJ2) tablero mensajes listaPilas numFichas = do
                                        f1 <- fichasJ1 ?: value
                                        p1temp <- radioGJ1 ?: selection
                                        lP <- listaPilas ?: value
                                        let p1 = p1temp + 1
                                        if valida (p1,f1) lP 
                                            then
                                                do
                                                    let nuevaLPilas = actualiza_pila lP (p1,f1) 
                                                    listaPilas !: value =: nuevaLPilas
                                                    pinta_tablero tablero listaPilas numFichas
                                                    deshabJugador jugando
                                                    if terminado nuevaLPilas 
                                                        then 
                                                            do
                                                                return True
                                                        else
                                                            do
                                                                mensajes !!: [ text =: "Toma tus fichas, jugador activo", color =: green]
                                                                habJugador contrario
                                                                return False
                                            else
                                                do
                                                    jugada_incorrecta mensajes
                                                    return False
    
valida :: (Int, Int) -> [Int] -> Bool
valida (n,m) pila
            | n <= (length pila) && fichasOK n m pila = True
            | otherwise = False

            
fichasOK :: Int -> Int -> [Int] -> Bool
fichasOK 1 m pila = m <= (head pila)
fichasOK n m pila = fichasOK (n-1) m (tail pila)

actualiza_pila :: [Int] -> (Int, Int) -> [Int]
actualiza_pila  pila (1, fichas) =  (fichaspila - fichas) : (tail pila)
                                        where 
                                            fichaspila = head pila
actualiza_pila pila (n, fichas) = head pila : actualiza_pila (tail pila) (n-1, fichas)      

terminado :: [Int] -> Bool
terminado = foldr (\x y -> (x==0) && y) True

jugada_incorrecta :: Label -> IO()
jugada_incorrecta m = m !!: [ text =: "Jugada incorrecta! Prueba de nuevo"
                            , color =: red
                            ]

ganador :: Window -> String -> (Entry Int, Entry Int, Button) -> IO ()
ganador ventana jugador (pilas, fichas, boton) = do
                                            let mensaje = "El " ++ jugador ++ " ha ganado la partida!!!\n\n¿Quieres jugar otra?"
                                            partidaNueva <- yesNoMessageBox ventana "Partida terminada" mensaje
                                            if partidaNueva 
                                                then
                                                    do
                                                        mapM hab [pilas, fichas]
                                                        hab boton
                                                else
                                                    quit
                                                    
----------------------------------------------------------------------------

-- Funciones auxiliares --

deshab :: (Able a) => a -> IO ()
deshab x = x !: enabled =: False
        

hab :: (Able a) => a -> IO ()
hab x = do
             x !: enabled =: True

deshabJugador :: (Entry Int, RadioGroup, Button) -> IO ()
deshabJugador (a,_,b) = do
                            deshab a
                            deshab b
                            
habJugador :: (Entry Int, RadioGroup, Button) -> IO ()
habJugador (a,_,b) = do
                          hab a
                          hab b   

pinta_tablero :: Frame -> Var [Int] -> Var Int -> IO ()
pinta_tablero f listaP maxF = do
                            jugada <- listaP ?: value
                            max_ <- maxF ?: value
                            let ancho = length(jugada)
                            let listaBool = concat . transpose $ map (expande max_) jugada
                            listaCanvas <- mapM (accion f) listaBool
                            f !: layout =: matrix ancho listaCanvas
                            

expande :: Int -> Int -> [Bool]
expande n m = [ False | _ <- [1..(n-m)]] ++ [True | _ <- [1..m]]

accion :: Frame -> Bool -> IO Canvas
accion panel elem = do
                        lienzo <- canvas [ bgColor =: white, size =: sz 50 50] panel
                        if elem == True
                            then do 
                                oval (sz 40 40) [fillColor =: blue, position =: pt 25 25] lienzo
                                return lienzo
                            else do
                                return lienzo

botones_r :: Frame -> Var Int -> IO [RadioButton]
botones_r f v = do
                numBotones <- v ?: value
                listaBotones <- mapM (radio_b f) [1..numBotones]
                (head listaBotones) !: checked =: True
                return listaBotones
                
radio_b :: Frame -> Int -> IO RadioButton
radio_b f n = do
                let titulo = "Pila nº" ++ show(n)
                boton <- radioButton [ text =: titulo 
                                     , font =: ( bold . arial ) 20
                                     ] f
                return boton

menuArchivo :: Window -> Menu -> (Entry Int, Entry Int, Button) -> Label -> (Entry Int, RadioGroup, Button) -> (Entry Int, RadioGroup, Button) -> IO Menu
menuArchivo app barra params mens j1 j2 = do
            archivo <- submenu [ text =: "&Archivo" ] barra
            menuItem [ text =: "&Nueva partida"
                     , on action =: menuPartida app params mens j1 j2
                     ] archivo
            menuSep [] archivo
            menuItem [ text =: "&Salir"
                     , on action =: quit
                     ] archivo
            return archivo
            
menuAyuda :: Window -> Menu -> IO Menu
menuAyuda app barra = do
    ayuda <- submenu [ text =: "A&yuda"     
                     ] barra
    menuItem [text =: "Ayuda"
             , on action =: do
                            okMessageBox app "Ayuda.." "GOTO http://en.wikipedia.org/wiki/Nim"-- openBrowserOn "http://en.wikipedia.org/wiki/Nim",
                            return ()                                                         -- pero seguro que no teneis el paquete Hledger                
             ] ayuda
    menuSep [] ayuda
    menuItem [text =: "Acerca de"
             , on action =: acercaDe app
             ] ayuda
    return ayuda
    
    
acercaDe :: Window -> IO () 
acercaDe app = do 
                okMessageBox app "Acerca de ..." ("Juego NIM con GUI creado por:\n"
                                               ++ "\tJose J Guerrero\n"
                                               ++ "para la asignatura Ampliación de Programación")
                return ()

menuPartida :: Window -> (Entry Int, Entry Int, Button) -> Label -> (Entry Int, RadioGroup, Button) -> (Entry Int, RadioGroup, Button) -> IO ()
menuPartida ventana (pilas,fichas,boton) mens j1 j2 = do
                                            partidaNueva <- yesNoMessageBox ventana "Partida nueva" "¿Deseas dibujar un nuevo tablero\ny empezar de nuevo?"
                                            if partidaNueva 
                                                    then
                                                        do
                                                            mapM deshabJugador [j1,j2]
                                                            mapM hab [pilas, fichas]
                                                            hab boton
                                                            mens !!: [ text =: "Empieza la partida! Selecciona número de pilas y máximo de fichas"
                                                                     , color =: blue
                                                                     ]
                                                    else return ()
                                            return ()