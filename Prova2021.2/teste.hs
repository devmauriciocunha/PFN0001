
semVogal :: String -> String -> String 
semVogal [] _ = ""
semVogal _ [] = ""
semVogal (x:xs)(y:ys)
         |(x == y) = x:(semVogal xs ys)
         |otherwise = ' ':(semVogal xs ys)