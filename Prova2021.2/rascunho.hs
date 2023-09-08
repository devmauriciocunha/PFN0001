import System.IO



main = do putStr "Arquivo:"
          hFlush stdout
          nome <- getLine
          txt <- readFile nome
          let d = zip [1..] (lines txt)
          imprimir d

imprimir [] = putStrLn ""
imprimir ((n,l):ls) = do putStr (show n)
                         putStr ".\t"
                         putStrLn l
                         imprimir ls
