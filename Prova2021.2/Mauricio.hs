data Arvore a = Folha | Galho a (Arvore a) (Arvore a)
                deriving Show

a1 = Galho 8 (Galho 6 Folha Folha) (Galho 10 Folha Folha)

mapArv :: (a -> b) -> Arvore a -> Arvore b
mapArv funcao Folha          = Folha
mapArv funcao (Galho no e d) = (Galho (funcao no) (mapArv funcao e) (mapArv funcao d))

-- Podemos definir a função que será passada
dobra :: Int -> Int
dobra x = x*2

main = print("(" mapArv (\x -> 2 * x) a1 ")")
