data Arbol = Hoja Integer | Ramificacion Arbol Integer Arbol deriving Show
data Dir = Der | Izq 

esHoja :: Arbol -> Bool
esHoja (Hoja _) = True
esHoja _ = False

sumaNodos :: Arbol -> Integer
sumaNodos (Hoja h) = h
sumaNodos (Ramificacion aIzq nodo aDer) = nodo + sumaNodos aIzq + sumaNodos aDer

altura :: Arbol -> Integer 
altura (Hoja h) = 1
altura (Ramificacion aIzq nodo aDer) = 1 + max (altura aIzq) (altura aDer)	

pertenece :: Integer -> Arbol -> Bool
pertenece x (Hoja h) 
			| x == h = True
			| otherwise = False
pertenece x (Ramificacion aIzq nodo aDer) 
			| x == nodo = True
			| otherwise = (pertenece x aIzq) || (pertenece x aDer)

busqueda :: [Dir] -> Arbol -> Integer
busqueda [] (Hoja h) = h 	
busqueda [] (Ramificacion aIzq nodo aDer) = nodo 
busqueda (Izq:xs) (Ramificacion aIzq nodo aDer) = busqueda xs aIzq
busqueda (Der:xs) (Ramificacion aIzq nodo aDer) = busqueda xs aDer

espejar :: Arbol -> Arbol 
-- espejar (Ramificacion (Hoja h1) nodo (Hoja h2)) = (Ramificacion (Hoja h2) nodo (Hoja h1))
espejar (Hoja h) = Hoja h
espejar (Ramificacion aIzq nodo aDer) = Ramificacion (espejar aDer) nodo (espejar aIzq)