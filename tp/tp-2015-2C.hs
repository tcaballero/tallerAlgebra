-- DATOS Y SHOW
type Pixel = (Integer, Integer, Integer)
type PixelDelta = (Integer, Integer, Integer)
type Frame = [[Pixel]]

data Video = Iniciar Frame | Agregar Frame Video deriving Eq
instance Show Video
   where show (Iniciar f) = mostrarFrame f
         show (Agregar f v) = (mostrarFrame f) ++ "\n" ++ (show v)

type FrameComprimido = [(Integer, Integer, PixelDelta)]
data VideoComprimido = IniciarComp Frame | AgregarNormal Frame VideoComprimido | AgregarComprimido FrameComprimido VideoComprimido
instance Show VideoComprimido
   where show (IniciarComp f) = "INICIAL \n" ++ mostrarFrame f
         show (AgregarNormal f v) = "NO COMPRIMIDO \n" ++ (mostrarFrame f) ++ "\n" ++ (show v)
         show (AgregarComprimido f v) = "COMPRIMIDO \n" ++ (mostrarFrameComprimido f) ++ "\n" ++ (show v)

mostrarFrame :: Frame -> String
mostrarFrame [] = ""
mostrarFrame (x:xs) = (show x) ++ "\n" ++ (mostrarFrame xs)

mostrarFrameComprimido :: FrameComprimido -> String
mostrarFrameComprimido [] = ""
mostrarFrameComprimido (x:xs) = "\t" ++ (show x) ++ "\n" ++ (mostrarFrameComprimido xs)

-- Ejercicio 1/5
ultimoFrame :: Video -> Frame
ultimoFrame = error "Implementar!!! (ejercicio 1)"
-- *Main> ultimoFrame video0 == f1
-- True

-- Ejercicio 2/5
norma :: (Integer, Integer, Integer) -> Float
norma = error "Implementar!!! (ejercicio 2)"
-- *Main> norma (10, 20, 30)
-- 37.416573

-- Ejercicio 3/5
pixelsDiferentesEnFrame :: Frame -> Frame -> Float -> FrameComprimido
pixelsDiferentesEnFrame = error "Implementar!!! (ejercicio 3)"
-- *Main> pixelsDiferentesEnFrame v1f1 v2f2 1
-- [(0,0,(3,3,3)),(0,1,(3,3,3)),(1,0,(3,3,3)),(1,2,(-3,-3,-3)),(2,1,(-3,-3,-3)),(2,2,(-3,-3,-3))]


-- Ejercicio 4/5
comprimir :: Video -> Float -> Integer -> VideoComprimido
comprimir = error "Implementar!!! (ejercicio 4)"

-- Ejercicio 5/5
descomprimir :: VideoComprimido -> Video
descomprimir = error "Implementar!!! (ejercicio 5)"


-- Funciones provistas por la cátedra
sumarCambios :: FrameComprimido -> FrameComprimido -> FrameComprimido
sumarCambios fc1 fc2 = [(i, j, sumar deltas (busqueda i j fc2)) | (i, j, deltas) <- fc1] ++
                       [(i, j, deltas) | (i, j, deltas) <- fc2, busqueda i j fc1 == (0,0,0)]
-- *Main> sumarCambios [(1,1,(2,2,2)),(2,2,(0,0,-1))] [(1,1,(-3,-3,-3)), (1,2,(1,1,1))]
-- [(1,1,(-1,-1,-1)),(2,2,(0,0,-1)),(1,2,(1,1,1))]

aplicarCambio :: Frame -> FrameComprimido -> Frame
aplicarCambio f fc = [ [nuevoVal f i j fc| j <- [0..length (f !! i) - 1]] | i <- [0..length f - 1]]
  where nuevoVal f i j fc = sumar ((f !! i) !! j) (busqueda (fromIntegral i) (fromIntegral j) fc)
--  *Main> aplicarCambio [[(1,1,1),(2,2,2)],[(3,3,3),(4,4,4)]] [(0, 1, (1,2,3))]
--  [[(1,1,1),(3,4,5)],[(3,3,3),(4,4,4)]]

busqueda :: Integer -> Integer -> FrameComprimido -> PixelDelta
busqueda i j [] = (0, 0, 0)
busqueda i j ((x, y, c) : cs) | x == i && j == y = c
                            | otherwise = busqueda i j cs

sumar :: PixelDelta -> PixelDelta -> PixelDelta
sumar (x,y,z) (x2,y2,z2) =  (x+x2,y+y2,z+z2)

-- PRUEBAS
p3 :: Pixel
p3 = (3,3,3)

p0 :: Pixel
p0 = (0,0,0)

-- Video 0:
f0 = [[p0, p0, p0], [p3, p3, p3]]
f1 = [[p3, p3, p3], [p3, p3, p3]]
video0 = Agregar f1 (Agregar f0 (Iniciar f0))

-- Video 1:  En la versión comprimida, todos los frames son comprimidos (salvo el inicial)

v1f1 :: Frame
v1f1 = [[p3, p3, p0, p0, p0],
	  [p3, p3, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v1f2 :: Frame
v1f2 = [[p0, p0, p0, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v1f3 :: Frame
v1f3 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p0, p0, p0]]

v1f4 :: Frame
v1f4 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p3, p3],
	  [p0, p0, p0, p3, p3]]


v1 :: Video
v1 = Agregar v1f4 (Agregar v1f3 (Agregar v1f2 (Iniciar v1f1)))

v1Comp :: VideoComprimido
v1Comp = comprimir v1 1 6


-- Video 2:  En la versión comprimida, sólo los frames 2 y 4 son comprimidos

v2f1 :: Frame
v2f1 = [[p3, p3, p0, p0, p0],
	  [p3, p3, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v2f2 :: Frame
v2f2 = [[p0, p0, p0, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v2f3 :: Frame
v2f3 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p3, p3, p3],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p0, p0, p0]]

v2f4 :: Frame
v2f4 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p3],
	  [p0, p0, p0, p3, p3],
	  [p0, p0, p0, p3, p3]]


v2 :: Video
v2 = Agregar v2f4 (Agregar v2f3 (Agregar v2f2 (Iniciar v2f1)))

v2Comp :: VideoComprimido
v2Comp = comprimir v2 1 6
