data Figura = Rectangulo Float Float Float Float | 
			  Circulo Float Float Float deriving Show 

c1 :: Figura 	
c1 = Circulo pi 0.0 0.0

r1 :: Float -> Figura 
r1 x = Rectangulo 0.0 0.0 (cos(45)*x) (sin(45)*x) 

area :: Figura -> Float
area (Circulo r x y) = pi * r**2
area (Rectangulo x1 y1 x2 y2) = (x2-x1)*(y2-y1)

data Punto = Point Float Float
data Figura2 = Rectangulo2 Punto Punto |
			   Circulo2 Punto Float
c2 :: Figura2 
c2 = Circulo2 (Point 0.0 0.0) pi

r2 :: Float -> Figura2
r2 x = Rectangulo2 (Point 0.0 0.0) (Point (cos(45)*x) (sin(45)*x))

area2 :: Figura2 -> Float
area2 (Circulo2 (Point x y) r) = pi * r**2
area2 (Rectangulo2 (Point x1 y1) (Point x2 y2)) = (x2-x1)*(y2-y1)

data ProgAritmetica = Vacio | CongruentesA Integer Integer

esMultiplo :: Integer -> Integer -> Bool
esMultiplo a b = (mod a b == 0)

pertenece ::  Integer -> ProgAritmetica -> Bool
pertenece n Vacio = False
pertenece n (CongruentesA a b)= esMultiplo (n-a) b

--incluido :: ProgAritmetica -> ProgAritmetica -> Bool
--incluido 

instance Show ProgAritmetica where
	show Vacio = "{}"
	show (CongruentesA a b) = "{a en Z | a = " ++ (show a) ++ "(mod " ++ (show b) ++ ")}"