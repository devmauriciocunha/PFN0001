--prova_final

{--[1.0] multiplos3ou5: Se listarmos todos os números naturais menores que 10 que são múltiplos de 3
ou 5, teremos 3, 5, 6 e 9. A soma destes números é 23. Desenvolva uma função que retorne a soma
de todos os múltiplos de 3 ou 5 menores que 1000.
multiplos3ou5 :: Int
Entrada: não há.
Saída: a soma dos múltiplos de 3 ou 5 menores de 1000.
Importante:
• Essa função não tem um parâmetro de entrada, já que o valor a ser calculado é fixo.
• Não devem ser utilizadas funções padrões de bibliotecas Haskell, exceto funções de ordem
superior.
• Sugere-se utilizar compreensão de listas na resolução deste problema.--}

{--somaM  :: [ Int ] ->  Int
somaM []  =  0
somaM (x : xs) = x +  soma xs

somaM [ x | x <- [1..9999], mod x 3 == 0 && mod x == 0 ]--}

somaMultiplos = somaTodos ([ x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0 ])

somaTodos [] = 0
somaTodos (x:xs) =  x + somaTodos xs 

main = do
  print(somaMultiplos)
  

{--[2.5] velasBoloAniversario: Você é responsável pelo bolo de aniversário de uma criança. Você decidiu
que o bolo terá uma vela para cada ano de sua idade total. As crianças só serão capazes de apagar as
velas mais altas. Dada uma lista de velas de tipo [Int], conte quantas são as velas mais altas.

Exemplo: velas = [4,4,1,3]
A altura máxima das velas é de 4 unidades de altura. Existem 2 delas, então deve ser retornado 2.
velasBoloAniversario :: [Int] → Int
Entrada: lista com as alturas das velas.
Saída: o número de velas que tem maior tamanho.
Importante:
• É possível utilizar funções auxiliares no desenvolvimento deste problema.
• Não devem ser utilizadas funções padrões de bibliotecas Haskell, exceto funções de ordem
superior.--}

velasBoloAniversario list = length (vezesVela(maiorVela list) list)

  where 
  
    maiorVela [x] = x
    maiorVela (x:xs) = 
      if x > maiorVela xs 
        then x
      else maiorVela xs

    vezesVela _ [] = []
    vezesVela n (x:xs) = 
      if n == x 
        then x : vezesVela n xs
      else vezesVela n xs


{--5. [2.5] converterHorario: Dado um horário no formato 12 horas AM/PM, converta-o para o horário no
formato de 24 horas.
Exemplos:
Entrada: “12:01:00PM”
Saída: “12:01:00”
Entrada: “12:01:00AM”
Saída: “00:01:00”
Entrada: “07:05:45PM”
Saída: “19:05:45”
converterHorario :: [Char] → [Char]
Entrada: um horário no formato de 12 horas - hh:mm:ssAM ou hh:mm:ssPM
Saída: horário no formato de 24 horas - hh:mm:ss
Importante:
• 12:00:00AM no formato de 12 horas é 00:00:00 no formato de 24 horas;
• 12:00:00PM no formato de 12 horas é 12:00:00 no formato de 24 horas;
• É possível utilizar funções auxiliares no desenvolvimento deste problema;
• Não devem ser utilizadas funções padrões de bibliotecas Haskell, exceto funções de ordem
superior, funções da biblioteca Data.Char e funções read e show.
• Considere que a entrada está exatamente no formato indicado, sem espaços em branco ou letras
minúsculas.--}




