import System.IO
type Doc = String 
type Linha = String 
type Palavra = String 

construirIndice :: Doc -> [([Int],Palavra)]
main = do putStr "Arquivo:"
          hFlush stdout
          nome <- getLine
          txt <- readFile nome
          let d = zip [1..] (lines txt)
          imprimir d


lines :: Doc -> [Linha] 
linhas = do 
          putStr "Arquivo:"
          hFlush stdout
          nome <- getLine
          txt <- readFile nome
          imprimir 
  
numLinhas:: [Linha] -- ^ 
  -> [(Int, Linha)]
numLinhas = do
    putStr "Arquivo:"
    hFlush stdout
    nome <- getLine
    txt<-readFile
    let d = zip [1..](lines txt)
    imprimir d

numeraPalavra :: [(Int, String )] -> [(Int, String )]
numeraPalavra [] = []
numeraPalavra ((n,b):ys) = numeraPalavras n(words b)++ numeraPalavra ys 

numeraPalavras :: Int -> [String ] -> [(Int,[Char])]
numeraPalavras _ [] = []
numeraPalavras n(x:xs) = (n,x): numeraPalavras n xs

ordenar :: (Ord) -> [a] -> [a]
ordenar [] = []
ordenar (k:xk) = ordenar [x|x == k]

agrupar [] = []
agrupar lista = listaOrd lista (length lista)
agruparOrd lista j = lista 
agruparOrd lista n = agruparOrd (troca lista) (n-1)
troca [x] = [x]
troca (x:y:zs)
                     |x > y = y : troca (x:zs)
                     |otherwise = x:troca (y:zs)

