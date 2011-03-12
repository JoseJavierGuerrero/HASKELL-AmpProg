module Pila ( Pila      -- tipo transparente
            , vacia       -- :: Pila a
            , apilar      -- :: a -> Pila a -> Pila a
            , desapilar   -- :: Pila a -> Pila a
            , cima        -- :: Pila a -> a
            , esVacia     -- :: Pila a -> Bool
            , mapPila     -- :: (a -> b) -> Pila a -> Pila b
            , foldPila    -- :: (a -> b -> b) -> b -> Pila a -> b
            , foldPilaAc  -- :: (b -> a -> b) -> b -> Pila a -> b
             )
            where

data Pila a = Vacia
            | Nodo a (Pila a)
            --deriving Show -- ?? 
{-
El inconveniente de usar deriving show es que si queremos ocultar detalles de implementacion deriving show
imprimira los constructores, para evitarlo debemos crear una instancia de show con la precondicion de que el 
tipo base derive show tambien
-}




instance Show a => Show (Pila a) where
   show Vacia = "<>"
   show (Nodo x Vacia) = "<" ++ show x ++ ">"
   show (Nodo x p) = "<" ++ show x ++ "> " ++ show p


-- construtores exportados como funciones

vacia :: Pila a
vacia = Vacia

apilar :: a -> Pila a -> Pila a
apilar = Nodo

-- otras operaciones

desapilar :: Pila a -> Pila a
desapilar Vacia = error "desapilar: pila vacía"
desapilar (Nodo _ p) = p

cima :: Pila a -> a
cima Vacia = error "cima:: pila vacía"
cima (Nodo x _) = x

esVacia :: Pila a -> Bool
esVacia Vacia = True
esVacia _     = False

mapPila :: (a -> b) -> Pila a -> Pila b
mapPila f = aplicar
   where
      aplicar Vacia = Vacia
      aplicar (Nodo x p) = Nodo (f x) (aplicar p)

foldPila :: (a -> b -> b) -> b -> Pila a -> b
foldPila f z = plegar
   where
      plegar Vacia = z
      plegar (Nodo x p) = f x (plegar p)

foldPilaAc :: (b -> a -> b) -> b -> Pila a -> b
foldPilaAc f ini p = acumular p ini
   where
      acumular Vacia ac = ac
      acumular (Nodo x p) ac = acumular p (f ac x)
