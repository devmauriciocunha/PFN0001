--ex1--
ehTriangulo a b c
                 |(a + b) > c && (a + c) > b && (b + c) > a = True
                 |otherwise = False

--ex2--

tipoTriangulo d e f
                  | (d == e) && (f /= e && f /= d) || (f == e) && (d /= e && d /= f) || (d == f) && (e /= d && e /= f) = "isosceles"
                  | (d == e) && (f == e && f == d) = "equilatero"
                  | (d /= e) && (f /= e && f /= d) = "escaleno"

--ex03--

triangulo a b c
               | (a + b) < c || (a + c) < b || (b + c) < a = "nao eh um triangulo"
               | (a == b) && (c /= b && c /= a) || (c == b) && (a /= b && a /= c) || (a == c) && (b /= a && b /= c) = "isosceles"
               | (a == b) && (c == b && c == a) = "equilatero"
               | (a /= b) && (c /= b && c /= a) = "escaleno"
              
--ex04--

somaPares 0 = 0 
somaPares x = if mod x 2 == 0 then x + somaPares (x-1) else somaPares (x-1)

--ex05--

somaPot2m m 0 = m
somaPot2m m n = ((2^n)*m) + somaPot2m m (n-1)

--ex06--

primo' x 1 = 1
primo' x cont
             | rem x cont == 0 = 1 + primo' x (cont - 1)
             | otherwise = primo' x (cont - 1)

primo x = if (primo' x x > 2) then False else True

--ex07--

auxseriePI a b n = a * 4/b + if (n > b + 2) then auxseriePI (negate a) (b + 2) n else 0
seriePI n = auxseriePI 1 1 n
