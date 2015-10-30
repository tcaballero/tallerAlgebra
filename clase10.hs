data Polinomio = Mono Float Integer |
				 Suma Polinomio Polinomio |
				 Producto Polinomio Polinomio

evaluar :: Polinomio -> Float -> Float
evaluar (Mono coef grado) x = coef * (x^grado)
evaluar (Suma p1 p2) x = (evaluar p1 x) + (evaluar p2 x)
evaluar (Producto p1 p2) x = (evaluar p1 x) * (evaluar p2 x)

maxGrado :: Polinomio -> Integer
maxGrado (Mono coef grado) = grado
maxGrado (Suma p1 p2) = max (maxGrado p1) (maxGrado p2)

coeficientes :: Polinomio -> [Float]
coeficientes (Mono 0 _) = []
coeficientes (Mono coef grado) = [coef]
coeficientes (Suma p1 p2) 
		| maxGrado p1 == ((maxGrado p2) + 1) = coeficientes p2 ++ coeficientes p1
		| otherwise = coeficientes p2 ++ [0.0] ++ coeficientes p1 

x :: Polinomio
x = Mono 1 1

instance Num Polinomio where
		(+) p q = Suma p q
		(*) p q = Producto p q 
		negate p = Producto p (Mono (-1) 0)
		fromInteger n = Mono 0 0
		abs p = undefined
		signum p = undefined
--data Polinomio a = Mono a Integer |
--				   Suma (Polinomio a) (Polinomio a) |
--				   Producto (Polinomio a) (Polinomio a)

--evaluar :: Num a => Polinomio a -> Float -> Float
--evaluar (Mono coef expo) x = coef * (x^expo)
--evaluar (Suma p1 p2) x = (evaluar p1 x) + (evaluar p2 x)
--evaluar (Producto p1 p2) x = (evaluar p1 x) * (evaluar p2 x)
