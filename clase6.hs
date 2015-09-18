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