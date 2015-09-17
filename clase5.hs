-- POTENCIA --
potencia :: Float -> Integer -> Float
potencia _ 0 = 1
potencia a n = a*potencia a (n-1)
-- DIVISION --
division :: Integer -> Integer -> (Integer, Integer)
division a d
	| a < d = (0, a)
	| otherwise = (q' + 1, r')
		where (q', r') = division (a-d) d;
			  --q' = fst f;
			  --r' = snd f;
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
-- CAPICUA --
capicua :: [Integer] -> Bool
capicua lista = lista == reverso lista
-- SUMA --
sumaLista :: [Integer] -> [Integer] -> [Integer]
sumaLista [] [] = []
sumaLista list1 list2 = (head list1 + head list2) : sumaLista (tail list1) (tail list2)
-- PRODUCTO INTERNO --
prodInterno :: [Float] -> [Float] -> Float
prodInterno [] [] = 0
prodInterno list1 list2 = (head list1 * head list2) + prodInterno (tail list1) (tail list2)
-- DIVISION --
division' :: Integer -> Integer -> (Integer, Integer)
division' a d
 	| (a < 0) && (d > 0) && (r == 0) = (-q, 0)
	| (a < 0) && (d > 0) = (-q-1, d-r)
	| (a < 0) && (d < 0) && (r == 0) = (q, 0)
	| (a < 0) && (d < 0) = (q+1, -d-r)
	| (a > 0) && (d < 0) = (-q, r)
	| otherwise = (q, r)
		where (q, r) = division (abs a) (abs d)
--
noTieneDivisoresHasta :: Integer -> Integer -> Bool
noTieneDivisoresHasta m n
	| m == 2 = not (resto == 0)
	| otherwise = noTieneDivisoresHasta (m-1) n && not (resto == 0)
		where resto = mod n m
		-- where resto = snd (division' m n)
-- ES PRIMO? (Utilizando noTieneDivisoresHasta) --
esPrimo' :: Integer -> Bool
esPrimo' 2 = True
esPrimo' p = noTieneDivisoresHasta (p-1) p
