import System.Environment

main = cuenta 

cuenta :: IO ()
cuenta = do 
          xs <- getArgs
          putStrLn $ "He recibido " ++ show (length xs) ++ " parámetros:"
          sequence_ (map putStrLn xs)