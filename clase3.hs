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
