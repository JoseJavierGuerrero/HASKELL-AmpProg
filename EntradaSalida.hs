module EntradaSalida where

-- entrada salida en Haskell

import Data.Char
import System.Environment

leeTres = getChar >>= \ x ->
              getChar >>= \ y ->
              getChar >>= \ z ->
              return (ord x + ord z)

leeTres' =     
            do
                  x <- getChar
                  y <- getChar
                  z <- getChar
                  return (x,y,z)

{-
notación do 

el cuerpo de do admite
    - generador : patrón <- acción IO a
    - acciones return 
    - def locales: let patrón = expr

-}

leeDos :: IO (String, Int) -- lee el nombre de una persona y su edad
leeDos =
            do        
                    _ <- putStr "nombre: " --pero se puede quitar el generador porque no interesa
                    nombre <- getLine
                    putStr "edad: "
                    edad <- readLn
                    return (nombre, edad)

{-
misterio =
      do 
            x <- getX
            print x

getX =
      do
            return "hola"
            return (1+2+3)
            return head (map toLower "HEAD")
            return (True && False)
            return "adios"
-}

favorito = do 
                  putStr "lenguaje favorito? "
                  xs <- getLine
                  let ys = map toLower xs
                  if ys == "haskell"
                        then
                              do
                                      putStr "cual si no?"
                        else 
                               do
                                      putStr "nooooo"
                                      putStr ("haskell es mejor que " ++ xs)


favorito' = do 
                  putStr "lenguaje favorito? "
                  xs <- getLine
                  let ys = map toLower xs
                  if ys == "haskell"
                        then
                              do
                                      putStr "cual si no?"
                        else 
                               do
                                     putStr "piensal mejor"
                                     favorito'


main = muestraArgs

muestraArgs :: IO ()
muestraArgs = do 
                              xs <- getArgs
                              case xs of 
                                      [] -> putStrLn "No se han pasado argumentos"
                                      _ -> putStrLn ("Se han pasado los argumentos " ++ show xs)



-- MAnejo de excepciones

-- catch :: IO a -> (IOError -> IO a) -> IO a

leeEntero :: IO Integer
leeEntero = readLn 
                      `catch`
                            \ excepcion -> do  
                                  print excepcion
                                  return 0 -- valor por defecto

leeEntero2 :: IO Integer
leeEntero2 = readLn
                      `catch`
                          \ _ -> do
                                  putStrLn "El valor introducido no es un entero."
                                  putStrLn "Introduce otro, por favor."
                                  leeEntero2

leeEnteroOK3 :: IO Integer 
leeEnteroOK3 =	readLn
                          `catch`
                              \ e -> ioError e -- se puede eta-reducir


---------------- Ejercicio 1 ----------------

tres1 :: IO (Char, Char)
tres1 = getChar >>= \ x ->
              getChar >>= \ y ->
              getChar >>= \ z ->
              return (x,z)

tres2 :: IO [Char]
tres2 = getChar >>= \ x ->
              getChar >>= \ y ->
              getChar >>= \ z ->
              return [x,y,z]

tres3 :: IO Int
tres3 =    getChar >>= \ x ->
              getChar >>= \ y ->
              getChar >>= \ z ->
              return $ ord x + ord y + ord z

---------------- Ejercicio 2 ----------------

saludo :: IO ()
saludo = putStr "Como te llamas? " >>=  \ _ ->
              getLine >>= \ xs ->
              putStrLn $ "Hola " ++ xs ++ "!"

---------------- Ejercicio 3 ----------------

factorial :: IO ()
factorial =     putStr "Numero?: " >>=  \ _ ->
                    readLn >>= \x ->
                    return (fact x) >>= \res ->
                    putStrLn $ "Factorial: " ++ (show res)

fact :: Int -> Int
fact x
      | x == 0 = 1
      | otherwise = x * fact  (x-1)

---------------- Ejercicio 4 ----------------

leeCad :: IO ()
leeCad = getChar >>= \x -> if x == '.' 
              then 
                putChar '.' 
              else 
                leeCad >>= \ _ ->
                putChar x


---------------- Ejercicio 5 ----------------

tres1Do :: IO (Char, Char)
tres1Do =   do
              x <- getChar 
              _ <- getChar 
              z <- getChar
              return (x,z)

tres2Do :: IO [Char]
tres2Do =   do
              x <- getChar 
              y <- getChar 
              z <- getChar
              return [x,y,z]

tres3Do :: IO Int
tres3Do =   do
              x <- getChar 
              y <- getChar 
              z <- getChar
              return $ ord x + ord y + ord z

saludoDo :: IO ()
saludoDo = do 
            putStr "Como te llamas?"
            nombre <- getLine
            putStrLn $ "Hola " ++ nombre ++ "!"

factorialDo :: IO ()
factorialDo =    do
                    putStr "Numero?: "  
                    numero <- readLn
                    putStrLn $ "Factorial: " ++ (show (fact numero))

leeCadDo :: IO ()
leeCadDo = do 
            x <- getChar
            if x == '.'
              then 
                do
                 putChar '.'
              else
                do  
                 leeCad
                 putChar x

---------------- Ejercicio 6 ----------------

pregunta :: Read a => String -> IO a
pregunta xs = do 
                putStrLn xs
                res <- readLn
                return res

preguntas :: IO (String, Integer)
preguntas = do 
  nombre <- pregunta "¿como te llamas?"
  edad   <- pregunta "¿cuantos años tienes?"
  return (nombre,edad)

---------------- Ejercicio 7 ----------------

leeSecuencia :: Read a => (a -> Bool) -> IO [a]
leeSecuencia f = do 
                   x <- readLn
                   if f x 
                    then 
                      do
                        return []
                    else
                      do
                        xs <- leeSecuencia f
                        return $ x:xs

---------------- Ejercicio 9 ----------------

repiteRec :: Int -> IO a -> IO ()
repiteRec n io = if n > 0
                          then 
                              do 
                                    io
                                    repiteRec (n-1) io
                          else 
                              do
                                    return () 

repiteSeq :: Int -> IO a -> IO ()
repiteSeq n io = sequence_ $ replicate n io

---------------- Ejercicio 10 ----------------

secuencia_ :: [IO a] -> IO ()
secuencia_ [] = return ()
secuencia_ (x:xs) = x >>= \_ ->             
                              secuencia_ xs

secuencia_foldr :: [IO a] -> IO a
secuencia_foldr = foldr1 (\x y -> do {x;y}) 

secuencia_foldl :: [IO a] -> IO a
secuencia_foldl = foldl1 (\x y -> do {x;y})

---------------- Ejercicio 11 ----------------

when :: Bool -> IO () -> IO ()
when b io = if b then io else return ()


ejemploWhen = do 
                putStrLn "Dame un número: "
                x <- readLn
                when (x < 0) (putStrLn "Negativo")
                when (x ==  0) (putStrLn "Cero")
                when (x > 0) (putStrLn "Positivo")

--------------- Ejercicio 12 -----------------

preguntaOK :: Read a => String -> IO a
preguntaOK xs = do 
	putStrLn xs
	res <- readLn
	return res
	`catch`
	\ _ -> do     
		putStrLn "El dato introducido es incorrecto. Intentelo de nuevo: "
		preguntaOK xs 


preguntasX :: IO (String, Integer)
preguntasX = do 
  nombre <- preguntaOK "¿como te llamas?"
  edad   <- preguntaOK "¿cuantos años tienes?"
  return (nombre,edad)


intenta :: IO a -> IO (Either IOError a)
intenta io = do 
                      r <- io              
          `catch`
                           \l -> return (Left l)   -- se puede eta-reducir
