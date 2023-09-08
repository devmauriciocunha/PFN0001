import Lista2

-- 1
pertenceTest = 
  [pertence 3 [1, 4, 3, 2], 
  pertence 10 [1, 4, 3, 2] == False]

-- 2
intercessaoTest = 
  [intercessao [1, 3, 5, 7, 9] [2, 5, 3, 6, 9] == [3, 5, 9], 
  intercessao [1, 2] [4,5] == []]

-- 3
inversoListaTest = 
  [inversoLista [1, 2, 3, 4] == [4, 3, 2, 1], 
  inversoLista [20..30] == reverse [20..30]]

-- 4
nUltimosTest = 
  [nUltimos 3 [1, 2, 3, 4, 5, 6] == [4, 5, 6],
  nUltimos 3 [1, 2] == [1,2],
  nUltimos 0 [1, 2, 3, 4, 5, 6] == []]

-- 5
enesimoTest = 
  [enesimo 3 [10, 20, 30, 40, 50] == 30,
  enesimo 0 [1..10] == -1,
  enesimo 10 [10, 20, 30, 40, 50] == -1,
  enesimo 10 [1..10] == 10,
  enesimo 1 [1..10] == 1]

-- 6
repetirTest = 
  [repetir 4 10 == [10, 10, 10, 10],
  repetir 0 10 == [],
  repetir 100 1 == take 100 (repeat 1)]

-- 7
intercalacaoTest = 
  [intercalacao [10, 15, 17, 20] [1, 2, 13, 15, 22] == [1, 2, 10, 13, 15, 15, 17, 20, 22],
  intercalacao [1..10] [11..20] == [1..20],
  intercalacao [11..20] [1..10] == [1..20]]

-- 8
menorTest = 
  [menor [10, 4, 5, 3, 12] == 3,
  menor [1..10] == 1,
  menor (take 10 (repeat 1)) == 1]

-- 9
removerElemTest = 
  [removerElem 1 [2, 4, 1, 3, 2, 1] == [2, 4, 3, 2, 1],
  removerElem 10 [1..9] == [1..9],
  removerElem 9 [1..9] == [1..8],
  removerElem 1 [1..9] == [2..9]]

-- 10
ordenarListaTest = 
  [ordenarLista [32, 10, 23, 10, 12, 4] == [4, 10, 10, 12, 23, 32],
  ordenarLista [10,9..1] == [1..10]]

-- 11
insereElemTest = [insereElem 12 [6, 9, 10, 15, 20] == [6,9,10,12,15,20],
  insereElem 15 [6, 9, 10, 15, 20] == [6,9,10,15,20],
  insereElem 5 [6, 9, 10, 15, 20] == [5,6,9,10,15,20],
  insereElem 21 [6, 9, 10, 15, 20] == [6,9,10,15,20,21]]

-- 12
primeirosDuplasTest = 
  [primeirosDuplas [("a", 34), ("b", 80), ("c", 180)] == ["a","b","c"],
  primeirosDuplas (zip ['a'..'z'] [1..15]) == take 15 ['a'..'z']]

-- 13
somaDuplasTest = 
  [somaDuplas [(1,2), (3,4), (10, 23)] == [3, 7, 33],
  somaDuplas (zip [1..10] [1..10]) == [2*x | x <- [1..10]]]

-- 14
menoresDuplasTest = 
  [menoresDuplas [(1, 3), (5, 3), (8, 10), (3, 3)] == [(1, 3), (8, 10)],
  menoresDuplas [(2,1)] == []]

-- 15
separarDuplasTest = 
  [separarDuplas 9 [10, 3, 5, 17, 12, 4, 9 ] == ([3, 5, 4, 9], [10, 17, 12]),
  separarDuplas 0 [1..10] == ([],[1..10]),
  separarDuplas 11 [1..10] == ([1..10],[]),
  separarDuplas 9 [1..10] == ([1..9],[10])]


verifica [] = putStrLn ""
verifica ((n, t): nts) = do putStrLn ("Exercicio " ++ show n ++
                                        if t then " Ok" else " Falhou")
                            verifica nts                                        
      
main = verifica ex 