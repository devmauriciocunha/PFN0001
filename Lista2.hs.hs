import Data.Char

{-1. Declare uma função que verifica se um elemento pertente a uma lista, recebendo um
número e uma lista como parâmetro e retornando True se o elemento estiver na lista e False caso
contrário.-}

pertence x [] = False 
pertence x (y:ys) = if x == y then True else pertence x ys

{-2. Declare uma função que retorne a intercessão entre duas listas.-}

intercessao [] _ = []
intercessao _ [] = []
intercessao (x:xs) (y:ys) = if x == y then x:intercessao xs ys else intercessao xs ys

{-3. Declare uma função que retorne o inverso de uma lista.-}

inversoLista [] = []
inversoLista (x:xs) = inverso xs ++ [x]

{-4. Declare uma função que retorne os n últimos elementos de uma lista.-}

nprimeiros n (x:xs) = if n > 0 then x:nprimeiros (n-1) xs else []
nUltimos n (x:xs) = inverso(nprimeiros n (inverso (x:xs)))

{-5. Declare uma função que receba um número n e uma lista e retorne o n ésimo elemento da
lista. Se n for maior que o tamanho da lista, retorne -1-}

enesimo n [] = -1
enesimo n (x:xs) = if n == 1 then x else enesimo (n-1) xs


{-6. Declare uma função que receba um inteiro n e um elemento e e crie uma lista com n elementos e.-}

repetir n e = if n > 0 then e:repetir (n-1) e else []

{-7. Declare uma função que receba duas listas previamente ordenadas e faça a intercalação (merge) dos elementos tendo como resultado a junção das duas listas em uma lista também ordenada.-}

intercalacao [] _ = []
intercalacao _ [] = []
intercalacao (x:xs) (y:ys) = if x < y then x:intercalacao xs (y:ys) else y:intercalacao (x:xs) ys

{-8. Declare uma função que retorne o menor valor de uma lista.-}

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

{-9. Declare uma função que receba uma lista e um elemento e retorne a lista sem a primeira ocorrência desse elemento.-}

removerElem _ [] = [] 
removerElem n (x:xs) = if n == x then xs else x:removerElem n xs

{-10. Usando as funções anteriores, declare uma função que ordene os elementos de uma
lista-}

ordenarLista [] = []
ordenarLista (x:xs) = menor (x:xs):ordenar (removerElem (menor (x:xs)) (x:xs))

{-11. Declare uma função que receba um elemento é uma lista ordenada insira este elemento
na lista colocando o na posição correta, ou seja, a lista resultante deve estar ordenada. Se o elemento
já pertencer à lista, ele não deve ser incluído.-}

insereElem n (x:xs) | n < x = n:(x:xs)
                    | n > x = x:ins n xs
                    | n == x = xs 

{-12. Declare uma função que receba uma lista de duplas [(a,b)], e retorne uma lista com
o primeiro elemento de cada dupla [a].-}

primeirosDuplas [] = []
primeirosDuplas ((a,b):xs) = a:primeiros xs

{-13. Declare uma função que receba uma lista de duplas e retorne uma lista com a soma dos
elementos de cada dupla.-}

somaDuplas [] = []
somaDuplas ((a,b):xs) = (a+b):somarDuplas xs

{-14. Declare uma função que recebe uma lista de duplas e retorna uma lista contendo
todas as duplas cujo primeiro elemento seja menor que o segundo.-}

menoresDuplas [] = []
menoresDuplas ((a,b):xs) = if a < b then(a,b):
 menoresDuplas xs else menoresDuplas xs

{-15. Declare uma função que receba um valor v e uma lista, a função deve retornar uma
dupla de listas, a primeira lista deve conter os elementos que são menores ou iguais a v e a segunda
lista deve retornar os elementos maiores que v.-}

menorV v [] = []
menorV v (x:xs) = if v >= x then x:menorV v xs else menorV v xs

maiorV v [] = []
maiorV v (x:xs) = if v < x then x:maiorV v xs else maiorV v xs

separarDuplas v [] = ([],[])
separarDuplas v (x:xs) = (menorV v (x:xs), maiorV v (x:xs))

{-16. Implemente o Algoritmo de Euclides Estendido, tal como a função gcd disponível em Prelude. O
Algoritmo de Euclides estendido permite calcular o máximo divisor comum (MDC) fornecendo como
resultado os coeficientes.-}

eMdc a 0 = (1, 0, a)
eMdc a b = (y, x - (div a b) * y, d)
     where 
         (x, y, d) = eMdc b (mod a b)

{-17. Declare uma função que receba uma lista de duplas [(x,y)], e retorne uma lista com o
inverso de cada dupla, ou seja [(y,x)]-}

inversoDupla [] = []
inversoDupla ((a,b):xs) = (b,a):inverso xs

{-18. Declare uma função que receba uma lista de duplas, e retorne lista indicando se os
elementos são iguais ou não (True/False).-}

simetrico [] = []
simetrico ((a,b):xs) = if a == b then True:simetrico xs else False:simetrico xs

{-19. Declare uma função que recebe um número inteiro, e retorna uma lista de duplas de inteiros
distintos (x,y) tal que 1 ≤ x, y ≤ i.-}

pares n = [(a,b) | a <- [1..n], b <- [1..n], a /= b]

{-20. Suponha que a sequência de um DNA é formado pelas letras A, T, C e G. Escreva uma
função que recebe uma sequência de DNA, e retorne a sequência invertendo as letras, tal que: A será
T e T será A, C será G e G será C. Ao final, toda a sequência deve também ser invertida.-}

inverterDNA [] = []
inverterDNA (x:xs) 
                   | x == 'A' = 'T':inverteDNA xs 
                   | x == 'T' = 'A':inverteDNA xs
                   | x == 'C' = 'G':inverteDNA xs
                   | x == 'G' = 'C':inverteDNA xs

inverteDNA [] = []
inverteDNA (x:xs) = reverse (inverterDNA (x:xs))

{-21. Desenvolva uma função em Haskell que permita calcular o troco em moedas para o café.
Para isso, a função deve receber o valor do café (Int) e o valor em dinheiro pago pelo cliente (Int), e retornará uma lista de tuplas [(a, b)], tal que a é o valor da moeda, e b a quantidade de moedas deste
valor.-}

moedas = [50, 20, 10, 5]
trocoCafe :: int -> int -> [(int, int)]
trocoCafe' :: int -> [int] -> [(int, int)]
trocoCafe vC vM =

{-22. Desenvolva uma função em Haskell que recebe e retorna uma string, com o comportamento
abaixo:-}


x:'A'
xs:'B':'C':'D':[] 
lenght (x:xs) = 4
take 4 (repeat x)

x: 'B' 
xs: 'C':'D':[] 
lenght (x:xs) = 3 
take 3 (repeat x)

