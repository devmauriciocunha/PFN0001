-- Aluno: Maurício Nascimento Cunha
import Data.Char
import Data.List

-- auxiliares 
maiores [] e = []
maiores (x:xs) e = if x > e then x:maiores xs e else maiores xs e
menores [] e = []
menores (x:xs) e = if x < e then x:menores xs e else menores xs e


{-Questão 1-}
maioresMenores (x:xs) e = ((menores (x:xs) e),(maiores (x:xs) e))



{-Questão 2-}
marcar' :: String -> String -> String 
marcar' x y = [if elem n y then n else '*' | n <- x]

marcar :: String -> String -> String 
marcar [] _ = ""
marcar _ [] = ""
marcar (x:xs)(y:ys)
         |(x == y) = x:(marcar xs ys)
         |otherwise = '*':(marcar xs ys)



{-Questão 3-}
inserir :: Ord a => a -> [a] -> [a]
inserir x [] = [x]
inserir x (y:ys) | x <= y    = x:y:ys
                | otherwise = y : inserir x ys
                


{-Questão 4-}
esta :: Eq a => a -> [a] -> Bool
esta a [] = False
esta a (x:z) = if (a == x) then True
                               else esta a z

intercalacao :: Eq a => [a] -> [a] -> [a]
intercalacao as bs = as ++ [b | b <- bs, not (esta b as)]



{-Questão 5-}
main = do
  line <- getLine
  if line == ""
    then return ()
    else do
      putStrLn (" contaPalavras = " ++ contaPalavras  line)
      main

contaPalavras  :: String -> String
contaPalavras  = show . length . words



{-Questão 6-}
