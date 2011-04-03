
module Tokeniser (tokens) where

import Data.Char


tokens :: (a -> Bool) -> [a] -> [[a]]

tokens _ [] = []
tokens f a =     if null(tok)
                      then 
                        tokens f resto
                      else
                        tok : tokens f resto
                          where 
                            aux = dropWhile (not.f) a
                            tok = takeWhile f aux
                            resto = dropWhile f aux

