-- POTENCIA --
potencia :: Float -> Integer -> Float
potencia _ 0 = 1
potencia a n = a*potencia a (n-1)
-- DIVISION --
division :: Integer -> Integer -> (Integer, Integer)
division a d
	| a < d = (0, a)
	| otherwise = (q' + 1, r')
		where f = division (a-d) d;
			  q' = fst f;
			  r' = snd f;
-- DIVISORES (PARCIAL) --
divParcial :: Integer -> Integer -> [Integer]
divParcial n 1 = [1]
divParcial n m
	| mod n m == 0 = dp ++ [m]
	| otherwise = dp
		where dp = divParcial n (m-1)
-- DIVISORES --
divisores :: Integer -> [Integer]
divisores n = divParcial n n
-- ES PRIMO --
esPrimo :: Integer -> Bool
esPrimo n = length (divisores n) == 2
-- SUMA --
suma :: [ Integer ] -> Integer
suma lista 
	| length lista == 0 = 0
	| otherwise = head lista + suma (tail lista)
-- PRODUCTORIA --
productoria :: [Integer] -> Integer
productoria lista 
	| length lista == 0 = 1
	| otherwise = head lista * productoria (tail lista)
-- REVERSO --
reverso :: [a] -> [a]
reverso lista 
	| length lista == 1 = lista
	| otherwise = reverso (tail lista) ++ [head lista]

