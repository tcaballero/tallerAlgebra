-- PERTENECE --
pertenece :: Integer -> [Integer] -> Bool
pertenece n lista
	| length lista == 0 = False
	| otherwise = pertenece n (tail lista) || (n == head lista)
-- REPETICION
hayRepeticion :: [Integer] -> Bool
hayRepeticion lista
	| length lista == 0 = False
	| pertenece (head lista) (tail lista) = True
	| otherwise = hayRepeticion (tail lista)
-- MENORES --
menores :: Integer -> [Integer] -> [Integer]
menores n lista
	| length lista == 0 = []
	| cabeza < n = cabeza : menorCola
	| otherwise = menorCola
		where menorCola = menores n (tail lista);
			  cabeza = head lista;
-- QUITAR --
quitar :: Integer -> [Integer] -> [Integer]
quitar n lista
	| length lista == 0 = []
	| cabeza == n = quitarCola
	| otherwise = cabeza : quitarCola
		where quitarCola = quitar n (tail lista);
			  cabeza = head lista;
-- MAXIMO --
maximo :: [Integer] -> Integer
maximo lista
	| length lista == 1 = cabeza
	| cabeza > maxCola = cabeza
	| otherwise = maxCola
		where cabeza = head lista;
			  maxCola = maximo (tail lista)
-- EN BASE --
enBase :: Integer -> Integer -> [Integer]
enBase b n
	| n == 1 = resto
	| otherwise =  enBase b (div n b) ++ resto
		where resto = [mod n b];
-- DE BASE --
deBase :: Integer -> [Integer] -> Integer
deBase b n
	| potencia == 0 = digito
	| otherwise = digito*b^(potencia) + deBase b (tail n)
		where potencia = (length n) - 1;
			  digito = head n

-- REVERSO --
reverso :: [a] -> [a]
reverso lista
	| length lista == 1 = lista
	| otherwise = reverso (tail lista) ++ [head lista]
-- CAPICUA --
capicua :: [Integer] -> Bool
capicua lista = lista == reverso lista
-- NÚMEROS DE LYCHREL --
capicuaPara :: [Integer] -> [Integer]
capicuaPara n
	| capicua n = n
	| otherwise = capicuaPara (enBase 10 ((deBase 10 n) + (deBase 10 (reverso n))))
