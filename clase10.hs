data Polinomio = Mono Float Integer |
				 Suma Polinomio Polinomio |
				 Producto Polinomio Polinomio

sumaListas :: Num a => [a] -> [a] -> [a]
sumaListas [] l = l
sumaListas l [] = l
sumaListas (x:xs) (y:ys) = (x + y): (sumaListas xs ys)

porListas :: Num a => [a] -> [a] -> [a]
porListas [] l = []
porListas l [] = []
porListas [x] (l:ls) = (x*l):(porListas [x] ls)
porListas (x:xs) l = sumaListas (porListas [x] l) (0:(porListas xs l))

evaluar :: Polinomio -> Float -> Float
evaluar (Mono a n) z = a*(z^n)
evaluar (Suma p q) z = (evaluar p z) + (evaluar q z)
evaluar (Producto p q) z = (evaluar p z) * (evaluar q z)

coeficientes :: Polinomio -> [Float]
coeficientes p = limpiarCeros (coeficientes' p)

coeficientes' (Mono a 0) = [a]
coeficientes' (Mono a n) = 0:(coeficientes' (Mono a (n-1)))
coeficientes' (Suma p q) = sumaListas (coeficientes' p) (coeficientes' q)
coeficientes' (Producto p q) = porListas (coeficientes' p) (coeficientes' q)

sacarCeros :: [Float] -> [Float]
sacarCeros (0:xs) = sacarCeros xs
sacarCeros l = l

limpiarCeros :: [Float] -> [Float]
limpiarCeros l = reverse (sacarCeros (reverse l))

instance Num Polinomio where
		(+) p q = Suma p q
		(*) p q = Producto p q
		negate p = Producto p (Mono (-1) 0)
		fromInteger n = Mono 0 0
		abs p = undefined
		signum p = undefined

instance Show Polinomio where
	show (Mono a n) = show a ++ "x^" ++ show n
	show (Suma p q) = show p ++ " + " ++ show q
--data Polinomio a = Mono a Integer |
--				   Suma (Polinomio a) (Polinomio a) |
--				   Producto (Polinomio a) (Polinomio a)

--evaluar :: Num a => Polinomio a -> Float -> Float
--evaluar (Mono coef expo) x = coef * (x^expo)
--evaluar (Suma p1 p2) x = (evaluar p1 x) + (evaluar p2 x)
--evaluar (Producto p1 p2) x = (evaluar p1 x) * (evaluar p2 x)
