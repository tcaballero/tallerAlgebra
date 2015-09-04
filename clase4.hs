-- FACTORIAL --
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)
-- FIBONACCI --
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib(n-1)+fib(n-2)
-- PAR --
par ::  Integer -> Bool
par 0 = True
par 1 = False
par n = par(n-2)
-- SUMA IMPARES --
sumaImpares :: Integer -> Integer 
sumaImpares 1 = 1
sumaImpares n = sumaImpares(n-1)+2*n-1
-- MULTIPLO DE 3 --
multiplo3 :: Integer -> Bool
multiplo3 0 = True
multiplo3 1 = False
multiplo3 2 = False
multiplo3 n = multiplo3 (n-3)
