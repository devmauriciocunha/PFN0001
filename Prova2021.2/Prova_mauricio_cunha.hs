import Data.List
import Data.Function


-----------------------------------------------------------------------------------------

-- Nome: Maurício Nascimento Cunha
-- Prova: 17.02.2022
-- PFN: 2021.2

-----------------------------------------------------------------------------------------
--funções a serem usadas pela questão 1--

separaMenor :: Ord a => [a] -> a -> [a]
separaMenor (x : xs) v = [a | a <- (x : xs), a < v]

separaMaior :: Ord a => [a] -> a -> [a]
separaMaior (x : xs) v = [a | a <- (x : xs), a > v]

-----------------------------------------------------------------------------------------

{-Questão 1-}
maioresMenores :: Ord a => [a] -> a -> ([a], [a])
maioresMenores (x : xs) v = (separaMenor (x : xs) v, separaMaior (x : xs) v)


-----------------------------------------------------------------------------------------

{-Questão 2-}
marcar :: [Char] -> Char -> [Char]
marcar [] a = []
marcar (x : xs) a
  | a /= x = x : marcar xs a
  | otherwise = '*' : marcar xs a

 -----------------------------------------------------------------------------------------

{-Questão 3-}
inserir :: Ord a => a -> [a] -> [a]
inserir a [] = [a]
inserir a (x:xs)
  | a < x = a : x : xs
  | otherwise = x : inserir a xs

 -----------------------------------------------------------------------------------------

{-Questão 4-}
intercalacao x [] = x
intercalacao [] y = y
intercalacao (x:xs) (y:ys) | x > y = y : intercalacao (x:xs) ys
                           | otherwise = x: intercalacao xs (y:ys)
 -----------------------------------------------------------------------------------------

 {-Questão 5-}
contaPalavras :: String -> Int
contaPalavras xs = length $ words xs

 -----------------------------------------------------------------------------------------

{-Questão 6-} 

data Arvore a = Folha | Galho a (Arvore a) (Arvore a)
    deriving Show

mapArv :: (a -> b) -> Arvore a -> Arvore b
mapArv f Folha = Folha
mapArv f (Galho x a1 a2) = (Galho (f x) (mapArv f a1) (mapArv f a2))