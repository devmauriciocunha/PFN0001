
{--1. Desenvolva a função geraPosicaoDosItens, que recebe uma lista de tarefas e prioridades, a posição inicial
que é 1, e retorna uma lista de tarefas com tipo: [(a,b,c)], onde a é nome da tarefa, b é a prioridade, e c a
ordem do elemento na lista.
geraPosicaoDosItens :: [([Char],Int)] -> Int -> [([Char],Int,Int)]
Ex: geraPosicaoDosItens [("Tarefa A",1),("Tarefa X",2),("Tarefa G",7),("Tarefa C",2),("Tarefa K",4)] 1
>> [("Tarefa A",1,1),("Tarefa X",2,2),("Tarefa G",7,3),("Tarefa C",2,4),("Tarefa K",4,5)]--}

geraPosicaoDosItens :: [([Char],Int)] -> Int -> [([Char],Int,Int)]

geraPosicaoDosItens []_ = []

geraPosicaoDosItens ((a,b):xs) n = (a,b,n) : geraPosicaoDosItens xs (n+1)


{--2. Desenvolva a função ordenaListaPelaPrioridade, que recebe uma lista de tipo [(a,b,c)] - conf. item 1, e
retorna uma lista de mesmo tipo ordenada pela prioridade.
ordenaListaPelaPrioridade :: [([Char],Int,Int)] -> [([Char],Int,Int)]
Ex: ordenaListaPelaPrioridade [("Tarefa A",1,1),("Tarefa X",2,2),("Tarefa G",7,3),("Tarefa C",2,4),("Tarefa
K",4,5)]
>> [("Tarefa A",1,1),("Tarefa X",2,2),("Tarefa C",2,4),("Tarefa K",4,5),("Tarefa G",7,3)]--}

ordenaListaPelaPrioridade :: [([Char],Int,Int)] -> [([Char],Int,Int)]

menor [(a,b,c)] = b
menor ((a,b,c): xs) = if b < menor xs then b else menor xs

ordenaListaPelaPrioridade [] = [] 
ordenaListaPelaPrioridade ((a,b,c):xs)= if b <= menor ((a,b,c):xs) then (a,b,c) : ordenaListaPelaPrioridade xs else ordenaListaPelaPrioridade(xs ++ [(a,b,c)])



{--3. Desenvolva a função atualizaPrioridade, que recebe uma lista de tipo [(a,b,c)] - conf. item 1 ordenada pela
prioridade, e a prioridade inicial que é 1, e retorna uma lista de mesmo tipo com as prioridades atualizadas,
iniciando por 1.
atualizaPrioridade :: [([Char],Int,Int)] -> Int -> [([Char],Int,Int)]
Ex: atualizaPrioridade [("Tarefa A",1,1),("Tarefa X",2,2),("Tarefa C",2,4),("Tarefa K",4,5),("Tarefa G",7,3)] 1
>> [("Tarefa A",1,1),("Tarefa X",2,2),("Tarefa C",2,4),("Tarefa K",3,5),("Tarefa G",4,3)]--}

atualizaPrioridade :: [([Char],Int,Int)] -> Int -> [([Char],Int,Int)]

atualizaPrioridade [] _ = []

atualizaPrioridade ((a,b,c):xs) n = if b == n then (a,b,c) : atualizaPrioridade xs n else (a, n+1, c) : atualizaPrioridade xs (n+1)

{--4. Desenvolva a função ordenaListaPelaOrdem, que recebe uma lista de tipo [(a,b,c)] - conf. item 1 com a
prioridade atualizada, e retorna uma lista de mesmo tipo ordenada pela sua posição original (terceiro
elemento da tupla).
ordenaListaPelaOrdem :: [([Char],Int,Int)] -> [([Char],Int,Int)]
Ex: ordenaListaPelaOrdem [("Tarefa A",1,1),("Tarefa X",2,2),("Tarefa C",2,4),("Tarefa K",3,5),("Tarefa G",4,3)]
>> [("Tarefa A",1,1),("Tarefa X",2,2),("Tarefa G",4,3),("Tarefa C",2,4),("Tarefa K",3,5) --}

ordenaListaPelaOrdem :: [([Char],Int,Int)] -> [([Char],Int)]

menor_ordem [(a,b,c)] = c 
menor_ordem ((a,b,c):xs) = if c < menor_ordem xs then c else menor_ordem xs



ordenaListaPelaOrdem [] = []
ordenaListaPelaOrdem ((a,b,c):xs) = if c <= menor_ordem ((a,b,c):xs) then (a,b) : ordenaListaPelaOrdem xs else ordenaListaPelaOrdem(xs ++ [(a,b,c)])


{--O teste final, será feito pela função imprimeListaAtualizada, cuja implementação está abaixo. Ela recebe uma
lista de tarefas e prioridades retorna uma lista de mesmo tipo, porém com a prioridade atualizada. O retorno
deve ser adaptado para o tipo indicado.
imprimeListaAtualizada :: [([Char],Int)] -> [([Char],Int)]
imprimeListaAtualizada xs = ordenaListaPelaOrdem (atualizaPrioridade (ordenaListaPelaPrioridade
(geraPosicaoDosItens xs 1)) 1)--}

imprimeListaAtualizada :: [([Char],Int)] -> [([Char],Int)]

imprimeListaAtualizada xs = ordenaListaPelaOrdem (atualizaPrioridade (ordenaListaPelaPrioridade(geraPosicaoDosItens xs 1)) 1)