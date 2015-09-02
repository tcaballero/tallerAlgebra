import Data.Char
-- CREAR PAR --
crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)
-- INVERTIR --
invertir :: (a, b) -> (b, a)
invertir t = (snd t, fst t)
-- DISTANCIA --
distancia :: (Float, Float) -> (Float, Float) -> Float
distancia p1 p2 = sqrt((fst p2 - fst p1)**2 + (snd p2 - snd p1)**2)
-- RAICES --
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = (((-b) + sqrt(b**2-4*a*c))/(2*a), ((-b) - sqrt(b**2-4*a*c))/(2*a))
-- LISTAR --
listar :: a -> a -> a -> [a]
listar x y z = [x, y, z]
-- RANGO DE PASO --
rangoDePaso :: Integer -> Integer -> Integer -> [Integer]
rangoDePaso n1 n2 n3 = [n1, n1 + n3 ..n2]
-- PENDIENTE --
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente p1 p2 = (snd p1- snd p2)/(fst p1 - fst p2)
-- INICIALES --
iniciales :: String -> String -> String
iniciales nom ape = (head nom : ".") ++ (head ape : ".")  
