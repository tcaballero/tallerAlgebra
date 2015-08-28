doble x = 2 * x
suma x y = x + y
normaVectorial v1 v2 = sqrt(v1 * v1 + v2 * v2)
funcionConstante8 x = 8
respuestaATodo = 42

signo n | n < 0 = -1
	| n > 0 = 1
	| otherwise = 0

abs1 n 	| n > 0 = n
	| otherwise = -n

maximo n1 n2 | n1 > n2 = n1
	     | otherwise = n2

maximo3 n1 n2 n3 = maximo (maximo n1 n2) n3 

yLogico v1 v2 = v1 && v2
oLogico v1 v2 = v1 || v2
noLogico v1 = not v1

f n1 n2 n3 | n2 < 10 = n1
	   | otherwise = n1 + n3

nand x y = not (x && y)

nor x y = not (x || y)

raices a b c = ((-b) + sqrt(b**2-4*a*c))/(2*a)
esPitagorica a b c = (a**2 + b**2 == c**2)
